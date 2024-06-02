# Automatic Tests for the Compilers Project -- CO24 @ LEIC-A

This repository maintains and runs a set of input-output tests for the Compilers project.

Full information can be found on the [official Fénix page](https://fenix.tecnico.ulisboa.pt/disciplinas/Com3/2023-2024/2-semestre) ([Projecto](https://fenix.tecnico.ulisboa.pt/disciplinas/Com3/2023-2024/2-semestre/projecto)) or on the [Compilers wiki](https://web.tecnico.ulisboa.pt/~david.matos/w/pt/index.php/Compiladores) ([Projecto](https://web.tecnico.ulisboa.pt/~david.matos/w/pt/index.php/Compiladores/Projecto_de_Compiladores)). 

## Structure

This repository is structured as follows:
* `auto-tests` -- test cases (input-output)
* `logs` -- per-group test results (`.res` is a summary, `.log` provides details)
* `sane` -- canonical materials (will replace the ones in individual projects)
* test scripts

## Test scripts 

The tests runner uses the main `run.sh` script to run all the tests.

The individual scripts have the following functions:
* `0-begin` -- prepare the new report
* `1-clone` -- fetch projects to be tested (by default, all in the `prj` sibling subgroup)
* `2-clean-ref` and `2-clean-work` -- remove extraneous materials
* `3-prepare` -- restore canonical materials and prepare for testing
* `4-test` -- run the input/output tests (uses the `test-group.sh` script)
* `5-summarize` -- analyse test results
* `6-end` -- close the report

The `test-group.sh` script tests a single project against a test suite.

A global summary of all tests for all groups is provided in [`GLOBAL-REPORT.md`](GLOBAL-REPORT.md).
Students whose result is **NO DELIVERY** (at each deadline) will be excluded from the course.

## Delivery results

* Entrega Inicial ("Entrega Zero"): [`GLOBAL-REPORT-e0.md`](GLOBAL-REPORT-e0.md)
* Entrega Intermédia: [`GLOBAL-REPORT-e1.md`](GLOBAL-REPORT-e1.md)
* Entrega Final: [`GLOBAL-REPORT-e2.md`](GLOBAL-REPORT-e2.md)

These files will become available as evaluation progresses.

## Forking this project

If you fork this project to run your tests, you will need to adapt the `1-clone` script: currently, it tries to clone all the projects in the `prj` subgroup. In general, this is not allowed for regular users.

## Support 

Questions should be directed to [co24@lst.inesc-id.pt](mailto:co24@lst.inesc-id.pt).

