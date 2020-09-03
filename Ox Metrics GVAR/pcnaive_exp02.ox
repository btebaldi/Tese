/*
C:\Users\Tebaldi\Dropbox\bruno-tebaldi\2019-01 Ox GVAR TaylorMade\pcnaive_exp02.ox
File created by PcNaive on 26-07-2019 14:19:22

*/
///////////////////////////////////////////////////////////
#include <oxstd.h>
// Use the PcGive class for estimation instead of PcFiml:
#define USE_PCGIVE
#import <packages/PcNaive/pcnaive_mc>

///////////////////////////////////////////////////////////
class CMyModel : CPcNaiveModel
{	// Allows customization for additional coefficients and tests
	CMyModel();                                // constructor

	virtual GetCoefName(const eval);
	virtual GetTestName(const eval);
	virtual GetTestIsTwoSided(const eval);
	virtual GetCoef(const eval);
	virtual GetTest(const eval);
};
CMyModel::CMyModel()
{
	CPcNaiveModel();     // call base class constructor
}
CMyModel::GetCoefName(const eval)
{	// return an array of strings with additional COEF_ names
	return {};
}
CMyModel::GetTestName(const eval)
{	// return an array of strings with additional TEST_ names
	return {};
}
CMyModel::GetTestIsTwoSided(const eval)
{	// return a row vector with a zero for each one-sided test,
	// and 1 for two-sided
	return <>;
}
CMyModel::GetCoef(const eval)
{	// return a row vector with the COEF_ estimates
	return <>;
}
CMyModel::GetTest(const eval)
{	// return a 2 x c matrix with the TEST_ statistics in the
	// first row, and p-values in the second row
	return <>;
}
///////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////
class CPcNaiveExp : CPcNaive
{
	CPcNaiveExp();					// constructor
	~CPcNaiveExp();					// destructor
	Generate(const iRep, const cT,	// generate replication
		const mxT);
	TransformY(const mY);
	TransformZ(const mZ);
};
CPcNaiveExp::CPcNaiveExp()
{
	CPcNaive(<50:[10]1000>+1, 20, 0, 500, 1, -1, 1, 
		<0.1,0.05>, 
		<0>);

	m_dgp = new CPcNaiveDgp(6, 3);	// create the DGP

	m_dgp.SetYParameterEcm(
<-0.5,0,0;
 0,0,0;
 0,-0.5,0;
 0,0,0;
 0,0,-0.5;
 0,0,0 >,
<1,0,-0.5;
 -1,0,0;
 0,1,-0.5;
 0,-1,0;
 0,0,1;
 0,0,0 >,
<0,0,0;
 0,0,0;
 0,0,0;
 0,0,0;
 0,0,0;
 0,0,0 >,
<1;
 2;
 3;
 4;
 5;
 6 >);
	m_dgp.SetZParameter(
<0.29999999999999999,0.25,0;
 0,0.40000000000000002,0;
 0,0,0.5 >,
<0;
 0;
 0 >,
<0;
 0;
 0 >);
	m_dgp.SetDistribution(U_DGP, NORMAL,
<0;
 0;
 0;
 0;
 0;
 0 >,
<1;
 1;
 1;
 1;
 1;
 1 >);
	m_dgp.SetDistribution(Z_DGP, NORMAL,
<0;
 0;
 0 >,
<1;
 1;
 1 >);

	SetRecursive(1);
	SetSave(oxfilename(2));

	m_sys = new CMyModel();			// create the system/model
	CPcNaive::CreateData(6, 3);		// create the system database

									// formulate the model
	m_sys.Select(Y_VAR, { "Ya", 0, 0} );
	m_sys.Select(U_VAR, { "Constant", 0, 0} );
	m_sys.Select(Y_VAR, { "Ya", 1, 1} );
	m_sys.Select(X_VAR, { "Yb", 0, 0} );
	m_sys.Select(X_VAR, { "Yb", 1, 1} );
	m_sys.Select(X_VAR, { "Yc", 0, 0} );
	m_sys.Select(X_VAR, { "Yc", 1, 1} );
	m_sys.Select(X_VAR, { "Yd", 0, 0} );
	m_sys.Select(X_VAR, { "Yd", 1, 1} );
	m_sys.Select(X_VAR, { "Ye", 0, 0} );
	m_sys.Select(X_VAR, { "Ye", 1, 1} );
	m_sys.Select(X_VAR, { "Yf", 0, 0} );
	m_sys.Select(X_VAR, { "Yf", 1, 1} );
	m_sys.Select(X_VAR, { "Za", 0, 0} );
	m_sys.Select(X_VAR, { "Za", 1, 1} );
	m_sys.Select(X_VAR, { "Zb", 0, 0} );
	m_sys.Select(X_VAR, { "Zb", 1, 1} );
	m_sys.Select(X_VAR, { "Zc", 0, 0} );
	m_sys.Select(X_VAR, { "Zc", 1, 1} );
	m_sys.SetMethod(M_OLS);

	m_sys.SetEval(50, 1000, 0);
	m_sys.AddEvalCoef(COEF_BETA, 0, 0);
	AddPlot(PLOT_RCOEF );
	SetPlotStep(0);

	Update();
	println("PcNaive run: ", oxfilename(0));
	Report(1, 1, 1);
}
CPcNaiveExp::~CPcNaiveExp()
{
	~CPcNaive();					// call base destructor
}

CPcNaiveExp::Generate(const iRep, const cT, const mxT)
{
	m_sys.RestoreModel();				// first reset to initial specification

	CPcNaive::Generate(iRep, cT, mxT);	// update data; set sample

	m_sys.Estimate();					// estimate the model
										// get and store the statistics:
	m_mTest = m_sys.GetEvalTest();		// 2nd row: p-values
	m_mCoef = m_sys.GetEvalCoef();		// row vector
	m_mCoef ~= m_sys.EvaluateSelection(m_dgp);
return TRUE;
}
CPcNaiveExp::TransformY(const mY)
{
// There are 2n custom vars, "CY0" "CY1",...
// mY is the T x n matrix with Y variables. Example:
// Renew( mY[][0] - mY[][1], "CY0");  // CY0 is Ya-Yb

}
CPcNaiveExp::TransformZ(const mZ)
{
// There are 2q custom vars, "CZ0" "CZ1",...
// mZ is the T x q matrix with Z variables. Example:
// Renew( mZ[][0] - mZ[][1], "CZ0");  // CZ0 is Za-Zb

}
///////////////////////////////////////////////////////////

main()
{
	decl exp = new CPcNaiveExp();
	exp.Simulate();
	delete exp;
}

// The next section is used by PcNaive to reload the design
// for interactive editing:
/**<PcNaive>**
::PcNaiveData
{
.m_bUsePcGiveDgp = 0;
.m_cY = 6;
.m_cZ = 3;
.m_cRank = 3;
.m_bVecmY = 1;
.m_bSimultaneousY = 0;
.m_bArmaYerrors = 0;
.m_bBreakY = 0;
.m_bYlag2 = 0;
.m_bCustomZforY = 0;
.m_bObsLoop = 0;
.m_iFreq = 1;
.m_iYear1 = 1960;
.m_iPeriod1 = 1;
.m_mxYlag = 1;
.m_mxZlag = 1;
.m_iTbreak = 10;
.m_iTbreakEnd = 20;
.m_bTransformDiff = 0;
.m_bTransformExp = 0;
.m_bTransformCust = 0;
.m_sTransformYcust = "// There are 2n custom vars, \"CY0\" \"CY1\",...\n// mY is the T x n matrix with Y variables. Example:\n// Renew( mY[][0] - mY[][1], \"CY0\");  // CY0 is Ya-Yb\n";
.m_sTransformZcust = "// There are 2q custom vars, \"CZ0\" \"CZ1\",...\n// mZ is the T x q matrix with Z variables. Example:\n// Renew( mZ[][0] - mZ[][1], \"CZ0\");  // CZ0 is Za-Zb\n";
.m_mA0 = <0,0,0,0,0,0;
 0,0,0,0,0,0;
 0,0,0,0,0,0;
 0,0,0,0,0,0;
 0,0,0,0,0,0;
 0,0,0,0,0,0 >;
.m_mA1 = <0,0,0,0,0,0;
 0,0,0,0,0,0;
 0,0,0,0,0,0;
 0,0,0,0,0,0;
 0,0,0,0,0,0;
 0,0,0,0,0,0 >;
.m_mA2 = <0,0,0;
 0,0,0;
 0,0,0;
 0,0,0;
 0,0,0;
 0,0,0 >;
.m_ma3 = <1;
 2;
 3;
 4;
 5;
 6 >;
.m_mA5 = <0,0,0,0,0,0;
 0,0,0,0,0,0;
 0,0,0,0,0,0;
 0,0,0,0,0,0;
 0,0,0,0,0,0;
 0,0,0,0,0,0 >;
.m_mY0 = <0,0,0,0,0,0 >;
.m_mA0b = <0,0,0,0,0,0;
 0,0,0,0,0,0;
 0,0,0,0,0,0;
 0,0,0,0,0,0;
 0,0,0,0,0,0;
 0,0,0,0,0,0 >;
.m_mA1b = <0,0,0,0,0,0;
 0,0,0,0,0,0;
 0,0,0,0,0,0;
 0,0,0,0,0,0;
 0,0,0,0,0,0;
 0,0,0,0,0,0 >;
.m_mA2b = <0,0,0;
 0,0,0;
 0,0,0;
 0,0,0;
 0,0,0;
 0,0,0 >;
.m_ma3b = <0;
 0;
 0;
 0;
 0;
 0 >;
.m_mA4b = <1,0,0,0,0,0;
 0,1,0,0,0,0;
 0,0,1,0,0,0;
 0,0,0,1,0,0;
 0,0,0,0,1,0;
 0,0,0,0,0,1 >;
.m_mAlpha = <-0.5,0,0;
 0,0,0;
 0,-0.5,0;
 0,0,0;
 0,0,-0.5;
 0,0,0 >;
.m_mBeta = <1,0,-0.5;
 -1,0,0;
 0,1,-0.5;
 0,-1,0;
 0,0,1;
 0,0,0 >;
.m_mAlphaB = <0,0,0;
 0,0,0;
 0,0,0;
 0,0,0;
 0,0,0;
 0,0,0 >;
.m_mBetaB = <0,0,0;
 0,0,0;
 0,0,0;
 0,0,0;
 0,0,0;
 0,0,0;
 0,0,0;
 0,0,0;
 0,0,0 >;
.m_mB0 = <0,0,0,0,0,0;
 0,0,0,0,0,0;
 0,0,0,0,0,0;
 0,0,0,0,0,0;
 0,0,0,0,0,0;
 0,0,0,0,0,0 >;
.m_mB1 = <0,0,0,0,0,0;
 0,0,0,0,0,0;
 0,0,0,0,0,0;
 0,0,0,0,0,0;
 0,0,0,0,0,0;
 0,0,0,0,0,0 >;
.m_mM0 = <0,0,0,0,0,0;
 0,0,0,0,0,0;
 0,0,0,0,0,0;
 0,0,0,0,0,0;
 0,0,0,0,0,0;
 0,0,0,0,0,0 >;
.m_mS0 = <1,0,0,0,0,0;
 0,1,0,0,0,0;
 0,0,1,0,0,0;
 0,0,0,1,0,0;
 0,0,0,0,1,0;
 0,0,0,0,0,1 >;
.m_mm0 = <0;
 0;
 0;
 0;
 0;
 0 >;
.m_ms0 = <1;
 1;
 1;
 1;
 1;
 1 >;
.m_mC0 = <0.29999999999999999,0.25,0;
 0,0.40000000000000002,0;
 0,0,0.5 >;
.m_mc1 = <0;
 0;
 0 >;
.m_mc2 = <0;
 0;
 0 >;
.m_mZ0 = <0,0,0 >;
.m_mM1 = <0,0,0;
 0,0,0;
 0,0,0 >;
.m_mS1 = <1,0,0;
 0,1,0;
 0,0,1 >;
.m_mm1 = <0;
 0;
 0 >;
.m_ms1 = <1;
 1;
 1 >;
.m_mPi = <0;
 0;
 0 >;
.m_mPiB = <0;
 0;
 0 >;
.m_sCustomZforY = "// mZC is added to the generated Zs before Y is generated.\n// mZC is the T x q matrix with customized Z variables. Example:\n// return zeros(cT,m_cZ-1) ~ range(1,cT)\';// m_cZ columns\nreturn 0;                                 // no custom Zs\n";
.m_iEdist = 1;
.m_iVdist = 1;
.m_mModel = <0,-1,0,1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8;
 -1,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1;
 0,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1;
 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0;
 0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1;
 0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0;
 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 >;
.m_sPvalues = "0.1,0.05";
.m_sTruePar = "0";
.m_sSampleSizes = "50:[10]1000";
.m_bRenewZ = 1;
.m_cTdiscard = 20;
.m_cTforc = 0;
.m_cRep = 1000;
.m_dSeed = -1;
.m_bUseCommon = 1;
.m_bEvalAsymp = 1;
.m_bSaveResults = 1;
.m_cPlotFreq = 0;
.m_bSetPlotFreq = 0;
.m_vCOEF = <0,1,0,0,0,0 >;
.m_vTEST = <0,0,0,0,0,0,0,0,0,0,0,0,0,0 >;
.m_vPLOT = <0,0,0,1,0,0,0,0,0,0,0,0 >;
.m_bAutometrics = 0;
.m_dAutoSize = 0.050000000000000003;
.m_iAutoOutliers = 0;
.m_bAutoChopLags = 1;
.m_bUsePcGiveForModel = 1;
}
**<PcNaive>**/

