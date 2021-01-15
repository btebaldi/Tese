#include <oxstd.oxh>
#import <packages/CATS/CATS>

main()
{
	//--- Ox code for CATS( 4)
	decl model = new CATS();

	model.Load("C:\\Users\\bteba\\Documents\\GitHub\\Tese\\Ox Metrics GVAR\\Database\\temp_DatabaseDesAdm_RA_v1.in7");
	model.Select("Y", {"R510_Admitidos", 0, 0});
	model.Select("Y", {"R510_Desligados", 0, 0});
	model.Select("Y", {"star_dem4", 0, 0});
	model.Select("Y", {"star_adm4", 0, 0});
//	model.Select("U", {"CSeason", 0, 0});
//	model.Select("U", {"CSeason_1", 0, 0});
//	model.Select("U", {"CSeason_2", 0, 0});
//	model.Select("U", {"CSeason_3", 0, 0});
//	model.Select("U", {"CSeason_4", 0, 0});
//	model.Select("U", {"CSeason_5", 0, 0});
//	model.Select("U", {"CSeason_6", 0, 0});
//	model.Select("U", {"CSeason_7", 0, 0});
//	model.Select("U", {"CSeason_8", 0, 0});
//	model.Select("U", {"CSeason_9", 0, 0});
//	model.Select("U", {"CSeason_10", 0, 0});
//	model.Select("U", {"Constant", 0, 0});

	model.Lags(13,13,13);
	model.I1Rank(2);
	model.Trend("DRIFT");
	model.Seasonals(1);
	model.SetSelSample(1996, 2, 2018, 12);
	model.SetMethod("RRR");
	model.Estimate();

	model.Restrict({"[beta]","[alpha]","* * 0 0"});
	model.BootstrapRestrictions();

	delete model;
}
