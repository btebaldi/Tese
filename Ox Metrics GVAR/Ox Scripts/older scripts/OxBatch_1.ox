#include <oxstd.oxh>
#import <packages/PcGive/pcgive>

main()
{
// This program requires a licenced version of PcGive Professional.
	//--- Ox code for SYS( 1)
	decl model = new PcGive();

	model.Load("C:\\Users\\Tebaldi\\Dropbox\\bruno-tebaldi\\2019-01 Ox GVAR TaylorMade\\Database\\GrandeRegiao.in7");
	model.Deterministic(3);
	// Allow for lagged seasonals
	model.Grow(-model.GetFrequency());

	model.Select("Y", {"R1_Desligados", 0, 0});
	model.Select("Y", {"R1_Admitidos", 0, 0});
	model.Select("Y", {"R1_Desligados", 1, 1});
	model.Select("Y", {"R1_Admitidos", 1, 1});
	model.Select("X", {"R2_Desligados", 0, 1});
	model.Select("X", {"R3_Desligados", 0, 1});
	model.Select("X", {"R4_Desligados", 0, 1});
	model.Select("X", {"R5_Desligados", 0, 1});
	model.Select("X", {"R2_Admitidos", 0, 1});
	model.Select("X", {"R3_Admitidos", 0, 1});
	model.Select("X", {"R4_Admitidos", 0, 1});
	model.Select("X", {"R5_Admitidos", 0, 1});
	model.Select("U", {"Constant", 0, 0});
	model.Select("U", {"CSeasonal", 0, 10});
	model.SetModelClass("SYSTEM");
	model.SetSelSample(2004, 2, 2016, 12);
	model.SetMethod("OLS");
	model.Estimate();
	model.TestSummary();

	delete model;
}
