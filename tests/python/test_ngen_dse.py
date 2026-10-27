import copy
import importlib.util
import json
from pathlib import Path
import subprocess
import tempfile
import unittest
from unittest.mock import patch

ROOT=Path(__file__).resolve().parents[2]
spec=importlib.util.spec_from_file_location('ngen_dse',ROOT/'scripts/ngen_dse.py')
dse=importlib.util.module_from_spec(spec);spec.loader.exec_module(dse)


class DirectDSETests(unittest.TestCase):
    def setUp(self):
        self.design=dict(schema=dse.SCHEMA,transform_size=16,streaming_width=2,
                         modulus='97',pe_count=1)

    def test_actual_values_to_cli_and_large_integer_preservation(self):
        c=dse.normalize({**self.design,'streaming_width':8,'pe_count':4})
        args=dse.arguments(c)
        self.assertEqual(args[args.index('-n')+1],'4')
        self.assertEqual(args[args.index('-k')+1],'3')
        self.assertEqual(args[args.index('-pe')+1],'4')
        large='18446744069414584321'
        self.assertEqual(dse.normalize({**self.design,'modulus':large})['modulus'],large)

    def test_reject_bad_requests_before_launch(self):
        for change in ({'task':'some-task'}, {'streaming_width':3}, {'pe_count':True},
                       {'modulus':True}, {'direction':'raintt'}, {'transform_size':1},
                       {'schema':'wrong'}, {'extra':1}, {'streaming_width':32}):
            with self.subTest(change=change),patch.object(dse,'invoke') as call:
                with self.assertRaises(ValueError):dse.evaluate({**self.design,**change})
                call.assert_not_called()

    def test_pipeline_controls_are_typed_and_forwarded(self):
        c=dse.normalize({**self.design,'multiplier_pipeline_stages':3,
                         'reduction_pipeline_stages':5,'output_register':True,'input_register':True,'bank_write_arbitration':'onehot'})
        args=dse.arguments(c)
        self.assertEqual(args[args.index('-multiplier-pipeline-stages')+1],'3')
        self.assertEqual(args[args.index('-reduction-pipeline-stages')+1],'5')
        self.assertIn('-output-register',args)
        self.assertIn('-input-register',args)
        self.assertEqual(args[args.index('-bank-write-arbitration')+1],'onehot')
        for change in ({'multiplier_pipeline_stages':2},{'multiplier_pipeline_stages':True},
                       {'reduction_pipeline_stages':0},{'output_register':1},{'input_register':1},{'bank_write_arbitration':'invalid'}):
            with self.assertRaises(ValueError):dse.normalize({**self.design,**change})

    def test_frame_overlap_is_boolean_and_forwarded(self):
        c=dse.normalize({**self.design,'frame_overlap':True})
        self.assertIn('-frame-overlap',dse.arguments(c))
        with self.assertRaises(ValueError):dse.normalize({**self.design,'frame_overlap':1})
        with self.assertRaises(ValueError):dse.normalize({**self.design,'architecture':'full-throughput','frame_overlap':False})

    def test_cyclic_does_not_receive_negacyclic_twist(self):
        c=dse.normalize({**self.design,'ring':'cyclic'})
        self.assertNotIn('-psi',dse.arguments(c))
        with self.assertRaises(ValueError):dse.normalize({**self.design,'ring':'cyclic','psi':'auto'})

    def test_backend_validation_stays_with_ngen(self):
        failure=subprocess.CompletedProcess([],1,'','stage-parallel rejects PE override')
        with patch.object(dse,'invoke',return_value=failure):
            r=dse.evaluate({**self.design,'architecture':'stage-parallel'})
        self.assertEqual(r['status'],'lowering-failed')
        self.assertFalse(r['qualified'])
        self.assertIn('PE override',r['stderr'])

    def test_timeout_is_not_a_qualified_result(self):
        with patch.object(dse,'invoke',side_effect=subprocess.TimeoutExpired([],1)):
            r=dse.evaluate(self.design)
        self.assertEqual(r['status'],'timeout')
        self.assertFalse(r['qualified'])

    def test_existing_output_is_never_overwritten(self):
        with tempfile.TemporaryDirectory() as folder,patch.object(dse,'invoke') as call:
            p=Path(folder)/'keep';p.write_text('keep')
            with self.assertRaises(FileExistsError):dse.evaluate(self.design,'generate',folder)
            self.assertEqual(p.read_text(),'keep');call.assert_not_called()

    def test_mismatched_metadata_rejected(self):
        metadata=dict(schema='ngen-design-v1',transform_size=8,streaming_width=2,
                      modulus='97',direction='forward')
        with patch.object(dse,'invoke',return_value=subprocess.CompletedProcess([],0,json.dumps(metadata),'')):
            r=dse.evaluate(self.design)
        self.assertEqual(r['status'],'artifact-error')

    def test_binary_change_during_generation_rejected(self):
        with tempfile.TemporaryDirectory() as folder:
            exe=Path(folder)/'ngen';exe.write_text('old')
            def change(*args):
                exe.write_text('new')
                return subprocess.CompletedProcess([],0,'{}','')
            with patch.object(dse,'invoke',side_effect=change):r=dse.evaluate(self.design,executable=exe)
            self.assertEqual(r['status'],'generator-changed')

    def test_real_parameter_workflow_without_task_catalog(self):
        with tempfile.TemporaryDirectory() as scratch:
            root=Path(scratch);request=root/'design.json';request.write_text(json.dumps(self.design))
            def cli(action,*extra):
                p=subprocess.run(['python3',str(ROOT/'scripts/ngen_dse.py'),action,
                                  '--spec',str(request),*map(str,extra)],cwd=root,
                                 capture_output=True,text=True,timeout=60)
                self.assertEqual(p.returncode,0,p.stdout+p.stderr)
                return json.loads(p.stdout)
            planned=cli('plan')
            generated=cli('generate','--output-dir',root/'out')
            self.assertEqual(planned['status'],'planned')
            self.assertEqual(generated['status'],'generated')
            self.assertEqual(planned['candidate_id'],generated['candidate_id'])
            self.assertFalse(generated['qualified'])
            self.assertTrue((root/'out/candidate.sv').is_file())
            self.assertEqual(generated,json.loads((root/'out/result.json').read_text()))
            pipelined={**self.design,'multiplier_pipeline_stages':3,
                       'reduction_pipeline_stages':5,'output_register':True,'input_register':True,'issue_register':True}
            request.write_text(json.dumps(pipelined))
            configured=cli('plan')
            self.assertEqual(configured['metadata']['architecture_parameters']['butterfly_pipeline_latency'],10)
            self.assertEqual(configured['metadata']['architecture_parameters']['output_register'],1)
            self.assertEqual(configured['metadata']['architecture_parameters']['input_register'],1)
            changed=copy.deepcopy(self.design);changed['pe_count']=2
            request.write_text(json.dumps(changed))
            self.assertNotEqual(planned['candidate_id'],cli('plan')['candidate_id'])
            # A stage-parallel request uses its own protocol and omits the PE axis.
            changed.pop('pe_count');changed.update(architecture='stage-parallel',protocol='next')
            request.write_text(json.dumps(changed));self.assertEqual(cli('plan')['status'],'planned')
            changed['direction']='inverse';request.write_text(json.dumps(changed))
            self.assertEqual(cli('plan')['metadata']['direction'],'inverse')
            changed.update(protocol='ready-valid',frame_overlap=True)
            request.write_text(json.dumps(changed))
            overlap=cli('plan')
            self.assertEqual(overlap['metadata']['initiation_interval'],8)
            self.assertTrue(overlap['metadata']['full_throughput'])
            changed.pop('frame_overlap');changed['architecture']='full-throughput'
            request.write_text(json.dumps(changed))
            alias=cli('plan')
            self.assertEqual(alias['metadata']['architecture'],overlap['metadata']['architecture'])
            self.assertEqual(alias['candidate_id'],overlap['candidate_id'])

    def test_capabilities_json_has_no_task_dependency(self):
        p=subprocess.run(['python3',str(ROOT/'scripts/ngen_dse.py'),'capabilities'],
                         capture_output=True,text=True,timeout=30)
        self.assertEqual(p.returncode,0,p.stderr)
        self.assertEqual(json.loads(p.stdout)['schema'],'ngen-capabilities-v1')


if __name__=='__main__':unittest.main()
