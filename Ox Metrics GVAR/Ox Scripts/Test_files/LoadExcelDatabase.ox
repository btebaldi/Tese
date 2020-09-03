#include <oxstd.oxh>
#import <packages/PcGive/pcgive_ects>

main()
{

	decl txDbaseMacroVariables, mMacroData, aMacoVarNames;
	txDbaseMacroVariables = "../../Database/MacroVariables.xlsx";

	println("Carregando dados de macrovariaveis");
	decl daBaseMacro = new Database();
	//daBaseMacro.LoadXls(txDbaseMacroVariables);
	daBaseMacro.Load(txDbaseMacroVariables);

	// println(daBaseMacro.GetAllNames());	
	// println(daBaseMacro.GetAll());

//	print( "%c", daBaseMacro.GetAllNames(), "%cf", daBaseMacro.GetAll());

	mMacroData = daBaseMacro.GetAll();
   	aMacoVarNames = daBaseMacro.GetAllNames();
	
//	println(daBaseMacro.GetVar({"PIM", "Selic"}));

	decl ii,aa;
	for(ii=0; ii<=5; ++ii)
	{
		if(ii == 0){
		aa = {sprint("R", ii, "_Adm")};
		}
		else
		{
		aa = aa | {sprint("R", ii, "_Adm")};
		}

		print(sprint("sas", aa));
		
	}
	
//	println({"PIM", "Selic"});

	delete daBaseMacro, txDbaseMacroVariables; 

}
