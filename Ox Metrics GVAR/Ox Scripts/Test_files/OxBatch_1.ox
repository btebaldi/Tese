#include <oxstd.oxh>
#import <packages/PcGive/pcgive>

main()
{
// This program requires a licenced version of PcGive Professional.
	//--- Ox code for EQ( 2)
	decl model = new PcGive();

	model.Load("C:\\Users\\bteba\\Documents\\GitHub\\Tese\\Ox Metrics GVAR\\Database\\MacroVariables.in7");
	model.Deterministic(-1);

	model.Select("Y", {"ln_cambio", 0, 1});
	// Formulation of the GUM (commented out)
/*
	model.DeSelect();
	model.Select("Y", {"ln_cambio", 0, 0});
	model.Select("X", {"Constant", 0, 0});
	model.Select("Y", {"ln_cambio", 1, 1});
	model.Select("X", {"ln_IPCA", 0, 1});
	model.Select("X", {"ln_oil", 0, 1});
	model.Select("X", {"Dln_IPCA", 0, 1});
	model.Select("X", {"EmpLiq", 0, 1});
	model.Autometrics(0.01, "none", 1);
*/
	model.SetSelSample(1995, 3, 2018, 12);
	model.SetMethod("OLS");
	model.AutometricsSet("print", 0);
	model.Estimate();
	model.InformationCriteria();

	delete model;
}
