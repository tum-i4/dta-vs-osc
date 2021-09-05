add_llvm_loadable_module(LLVMScVirt
	InfoPass.cpp
	AddAttributePass.cpp
	OpcodeGenerator.cpp
	CodeMap.cpp
	DataMap.cpp
	InstrOperandTypes.cpp
	ISATranslator.cpp
	VirtualizationBuilder.cpp

	ScVirt.cpp

	DEPENDS
	intrinsics_gen
	PLUGIN_TOOL
	opt
)
