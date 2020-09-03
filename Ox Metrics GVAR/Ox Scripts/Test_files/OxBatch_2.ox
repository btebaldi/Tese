#include <oxstd.oxh>
#import <packages/CATS/CATS>

main()
{
	//--- Ox code for CATS( 8)
	decl model = new CATS();

	model.Load("C:\\Program Files\\OxMetrics8\\CATS\\German-US-PPP\\PPPuip.in7");
	model.Select("Y", {"dp1c", 0, 0});
	model.Select("Y", {"dp2", 0, 0});
	model.Select("Y", {"b1", 0, 0});
	model.Select("Y", {"b2", 0, 0});
	model.Select("Y", {"ppp", 0, 0});
	model.Select("X", {"C(1991:1)", 0, 0});
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
//	model.Select("X", {"Constant", 0, 0});

	model.Lags(2,2,2);
	model.I1Rank(5);
	model.Trend("CIMEAN");
	model.Seasonals(1);
	model.SetSelSample(1975, 9, 1998, 12);
	model.SetMethod("RRR");
	model.Estimate();

	println("MY TRACE TEST");
	println(model.GetTraceTest());


	println(model.DoI1RankTable(0.05, TRUE));

	println("GET BETA");
	println(model.GetBeta());
							 println(model.GetBeta());

							 println("RANK ORDER");
							 println(model.m_iR);
							 
	
	delete model;
}
