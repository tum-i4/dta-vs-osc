# Artifact Submission for "Dynamic Taint Analysis versus Obfuscated Self-Checking"

To set up this project, a docker image can be built with the following command: `docker build -t vsc .`\
The built docker image includes the updated VirtSC, a dynamic taint analysis attack on the original VirtSC, as well as scripts and files that can be useful for the evaluation of VirtSC and its attack.

To create a new container using the image, use `docker run -it vsc bash`.

To open the container at a later point in time again, use `docker start -i [container id/name]`.


## VirtSC

The original version of VirtSC was created by Ahmadvand et al. [1]. The initially built version in this project is VirtSC after its security update.

To mark functions in the program's source code to be virtualized or additionally checked by guards, use either `__attribute__((sc_virtualize))` or `__attribute__((sc_sensitive))`, respectively.

The main files constituting VirtSC can be found in `/home/sip/sc-virt/src/lib/Transforms/ScVirt`.

The following example shows how to protect and compile a C++ program using VirtSC. Here, `main.cpp` would contain the source code, and the resulting binary file would be `main.out`:
```
/home/sip/built-sc-virt/bin/clang++ -c -emit-llvm main.cpp -o main.bc
/home/sip/built-sc-virt/bin/opt -reg2mem < main.bc > prep.bc
/home/sip/built-sc-virt/bin/opt -load LLVMScVirt.so -scvirt -connectivity=5 < prep.bc > out.bc
/home/sip/built-sc-virt/bin/clang++ -o main.out out.bc
```
The `connectivity` parameter determines the maximum number of guards for each sensitive function. To disassemble the protected LLVM output, use the following command and open `out.ll` with a text editor:
```
/home/sip/built-sc-virt/bin/llvm-dis -o out.ll out.bc
```

More information can be found in [VirtSC's original README](sc-virt/README.md).


## Dynamic Taint Analysis Attack on VirtSC

This section refers to the content of the directory `/home/sip/attacking_anti_tamper`.

For the attack, some of the files for Guggenmos' master's thesis [2] were used. The original project can be found [here](https://github.com/Toizi/attacking_anti_tamper).

To attack a protected binary, use `run.py`.
- Example usage: `./run.py evaluation/binaries_old_tampered/sha.out -o sha_data.txt --args="/home/sip/attacking_anti_tamper/evaluation/dataset/cmdline-args/sha_input_small.asc"`
- More command line arguments are available. However, it cannot be guaranteed that they work as intended, as they have not been used after modifying Guggenmos' code.
- The script uses code that has been compiled from the directories `tracer` and `taint_cpp`. The [original attack's README](https://github.com/Toizi/attacking_anti_tamper/blob/master/README.MD) contains more details on how to build and use the individual components.


## Evaluation

This section refers to the content of the directory `/home/sip/attacking_anti_tamper/evaluation`.

### Dataset

The following folders constitute the dataset:
- `VirtSC_*`: The files that have to be replaced before recompiling to change the version of VirtSC.
- `dataset`: The unprotected dataset of programs including any predefined input and which libraries have to be linked to.\
  Side note: The current command-line input for `susan` is not recognized by the program. The last token would have to be changed from `s` to `-s`.
- `binaries_*`: The dataset of programs protected by different versions of VirtSC, which was used for the evaluation in Valenzuela's bachelor's thesis [3].

### Scripts

- To create all binaries of the dataset using different VirtSC versions, use `create_all_binaries.sh`. As all options are covered here, no command-line flags are necessary. The script uses the following other scripts:
  - `build_virtsc_version.sh` builds a specific version of VirtSC.
  - `compile_dataset.sh` compiles the dataset of programs using the current version of VirtSC. It uses the script `protect.sh` which protects a single program of the dataset.
- To attack an entire version of VirtSC, use `attack_virtsc_version.sh`.
  - Example usage: `./attack_virtsc_version.sh binaries_old_tampered`
  - The script creates a directory with all attack data gathered by `run.py` in the directory `attack_data`. Already present data will not be collected again.
- To generate graphs for the time needed and the number of skipped instructions for each attack given the set of data generated by `run.py`, use `generate_attack_figure_time.py` and `generate_attack_figure_skipped.py`, respectively.
  - Example usage for time needed: `./generate_attack_figure_time.py -i attack_data/old_tampered -o figures`
  - Example usage for skipped instructions: `./generate_attack_figure_skipped.py -i attack_data/old_tampered -o figures`
- To generate charts comparing the execution time and file size of a set of programs protected by two versions of VirtSC, use `compare_time.py` and `compare_size.py`, respectively.
  - Before executing `compare_time.py`, the execution has to be timed using `time_execution.py`.
  - Example usage for execution time:\
    `./time_execution.py --old binaries_old_dummy_check --new binaries_new_dummy_check -o execution_times.txt`\
    `./compare_time.py -i execution_times.txt -o figures`
  - Example usage for file size: `./compare_size.py --old binaries_old_dummy_check --new binaries_new_dummy_check -o figures`
- To print a LaTeX table with an entire set of data generated by `run.py`, use `print_latex_table.py`.
  - Example usage: `./print_latex_table.py -i attack_data/old_tampered`
- To print a list of specific values that can be used to compute an average value, among others, use the other scripts with the prefix `print_`.
  - Example usage for attack data: `./print_value_lists.py -i attack_data/old_tampered`
  - Example usage for execution time:\
    `./time_execution.py --old binaries_old_dummy_check --new binaries_new_dummy_check -o execution_times.txt`\
    `./print_overhead_list_time.py -i execution_times.txt`
  - Example usage for file size: `./print_overhead_list_size.py --old binaries_old_dummy_check --new binaries_new_dummy_check`


## Bibliography

[1] M. Ahmadvand, D. Below, S. Banescu, and A. Pretschner. “VirtSC: Combining Virtualization Obfuscation with Self-Checksumming”. In: *Proceedings of the 3rd ACM Workshop on Software Protection*. 2019, pp. 53–63.

[2] M. Guggenmos. “Software Protection and Taint-based Attacks”. MA thesis. Technical University of Munich, 2020.

[3] S. Valenzuela. “Making Self-Checksumming Combined with Virtualization Obfuscation Resistant against Attacks”. BA thesis. Technical University of Munich, 2020.
