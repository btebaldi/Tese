#include <oxstd.oxh>

main()
{
	// enter code

decl iQtdVarDependente, iQtdVarMacro, iQtdLags, iQtdRegioes, txMatPathG_Matrix, txMatPathResult_Matrix, txMatPathRaw_Matrix, txMatPathW_Matrix;

	iQtdVarDependente = 2; // NAO MUDAR ISSO !!! (SE MUDAR FAZER REVISAO DO CODIGO)
	iQtdLags = 2;
	iQtdRegioes = 5; // Isso devia vir de um arquivo e configuracao!!!
	iQtdVarMacro = 3; // Isso poderia ser melhorado???!!!
	

	// Configuracao dos diretorios
	txMatPathG_Matrix = "../mat_files/G_Matrix/";
	txMatPathResult_Matrix = "../mat_files/Result_Matrix/";
	txMatPathRaw_Matrix = "../mat_files/RawMatrix/";
	txMatPathW_Matrix = "../mat_files/W_Matrix/";
	
	decl mG0, mGy, mGy_inv, mGyL, mGL, mC, iCurrentLag, mG_alpha, mIIS_Stacked, mU_Stacked, mD_Stacked, mD0_stk, mDL_stk, mH, mV, mHw, mHw_lag, mWw, mBw, mBWw, mAlphaBeta_w;

	// Carrega a matrix G0
	println("Carregando matriz G0");
	mG0 = loadmat(sprint(txMatPathG_Matrix, "G0.mat"));
	//println("mG0: ", mG0);

	// Carrega a matrix Stackeds
	println("Carregando matrizes stacked (Alpha, IIS, U, D)");
	mG_alpha = loadmat(sprint(txMatPathG_Matrix, "G_alpha.mat"));
	println("Matrix Alpha_Stacked.mat carregada");
	mIIS_Stacked = loadmat(sprint(txMatPathG_Matrix, "IIS_Stacked.mat"));
	println("Matrix IIS_Stacked.mat carregada");
	mU_Stacked = loadmat(sprint(txMatPathG_Matrix, "U_Stacked.mat"));
	println("Matrix U_Staked.mat carregada");
	mD_Stacked = loadmat(sprint(txMatPathG_Matrix, "D_Stacked.mat"));
  	println("Matrix D_Staked.mat carregada");

//	println(mD_Stacked);
	

	//Separacao da matriz D_stk em D0_stk e DL_stk
	mD0_stk = mD_Stacked[0:][0:iQtdVarMacro-1];
	//println("mD0_stk: ", mD0_stk);

	mGy = (unit(iQtdVarMacro) | -mD0_stk)~(zeros(iQtdVarMacro,columns(mG0)) | mG0);
	//println(mGy);

	mGy_inv = invert(mGy);
	
	// Salva as matrizes de resultado
	println("Salvando matrizes stacked (Alpha, IIS, U, D)");


	
	mHw = loadmat(sprint(txMatPathRaw_Matrix, "H_w", ".mat"));
  	println("Matrix H_w.mat carregada");

	mBw = loadmat(sprint(txMatPathRaw_Matrix, "B_w", ".mat"));
  	println("Matrix B_w.mat carregada");

	mWw = loadmat(sprint(txMatPathW_Matrix, "W_w", ".mat"));
  	println("Matrix W_w.mat carregada");

	println("mBw", mBw);

	println("mWw", mWw);


	for(iCurrentLag = 1; iCurrentLag <= iQtdLags; ++iCurrentLag)
	{
		// Carrega a matrix de constantes
		println("Carregando matrizes stacked (G", iCurrentLag,")");
		mGL = loadmat(sprint(txMatPathG_Matrix, "G", iCurrentLag, ".mat"));
		println("mGL (lag:", iCurrentLag,"): ", mGL);


		println("Costruindo matriz D_{stk; l} (lag:", iCurrentLag,")");
		mDL_stk	= mD_Stacked[][iQtdVarMacro*iCurrentLag:(iQtdVarMacro*(iCurrentLag+1))-1];
		println(sprint("mDL_stk", "(", iCurrentLag,")", ": "), mDL_stk);

		mHw = loadmat(sprint(txMatPathRaw_Matrix, "H_w", ".mat"));
//		println("begin:", iQtdVarMacro*(iCurrentLag-1));
//		println("end:", (iQtdVarMacro*iCurrentLag)-1);
		print(mHw);
		mHw	= mHw[][(iQtdVarMacro*(iCurrentLag-1)):((iQtdVarMacro*(iCurrentLag))-1)];

		mBWw = mBw[][iCurrentLag-1] * mWw;

		print("mBWw", mBWw);

		mGyL = (mHw | mDL_stk) ~ (mBWw | mGL);

		println(mGyL);
		
	}
}
