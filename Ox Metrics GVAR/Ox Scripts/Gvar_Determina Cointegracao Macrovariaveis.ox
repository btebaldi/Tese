#include <oxstd.oxh>
#import <packages/CATS/CATS>

main()
{
	//--- Ox code for CATS( 9)
	decl model = new CATS();

	model.Load("C:\\Users\\bteba\\Documents\\GitHub\\Tese\\Ox Metrics GVAR\\Database\\MacroVariables.in7");
	model.Select("Y", {"lpim_BR", 0, 0});
	model.Select("Y", {"ln_Selic_aa", 0, 0});
	model.Select("Y", {"ln_cambio", 0, 0});
	model.Select("Y", {"Dln_IPCA", 0, 0});
//	model.Select("U", {"Constant", 0, 0});

	model.Lags(2,2,2);
	model.I1Rank(1);
	model.Trend("DRIFT");
	model.SetSelSample(1995, 4, 2018, 12);
	model.SetMethod("RRR");
	model.Estimate();

	model.BootstrapRankTest();

	delete model;
}
