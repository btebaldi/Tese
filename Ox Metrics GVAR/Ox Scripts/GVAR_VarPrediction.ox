#include <oxstd.oxh>

main()
{
	#include "./Config.ini"

	decl mBeta1, mBeta2, mBeta3, mBeta4, mT, mI_1, mBeta3_tilda, mBeta4_tilda, Matrix, Matrix2;

	// Carrega a matrix G0
	println("Carregando matriz mGy_inv_X_mGyL1 (Beta1)");
	mBeta1 = loadmat(sprint(txMatPathResult_Matrix, "mGy_inv_X_mGyL1.mat"));
//	println("mBeta1: ", mBeta1);

	println("Carregando matriz mGy_inv_X_mGyL2 (Beta2)");
	//mBeta2 = loadmat(sprint(txMatPathResult_Matrix, "mGy_inv_X_mGyL2.mat"));
	mBeta2 = zeros(rows(mBeta1), columns(mBeta1));
//	println("mBeta2: ", mBeta2);

	println("Carregando matriz mGy_inv_X_mC (Beta3)");
	mBeta3 = loadmat(sprint(txMatPathResult_Matrix, "mGy_inv_X_mC.mat"));
//	println("mBeta3: ", mBeta3);
	
	println("Carregando matriz mGy_inv_X_mL (Beta4)");
	mBeta4 = loadmat(sprint(txMatPathResult_Matrix, "mGy_inv_X_mL.mat"));
	// println("mBeta4: ", mBeta4);

	// Como as variaveis IIS não são utilizadas no VAR entao fazemos uma selecão do beta3 para contemplar apenas a Const + seasonal
	// mBeta3 = mBeta3[][0:11];
	//	println("mBeta3: ", mBeta3);

	// Adicionamos uma coluna de zeros que sera responsavel pela ceasonal(12) que só existe no modelo por questoes de atualizacao
	// mBeta3_tilda = mBeta3 ~ zeros(13, 1);
	// println("mBeta3_tilda: ", mBeta3_tilda);
	

	// Construcao da matriz de Transicao
	mT = <    1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0;
			 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1;
			 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0;
			 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0;
			 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0;
			 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0;
			 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0;
			 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0;
			 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0;
			 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0;
			 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0;
			 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0;
			 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0>	;
	decl debug_cols;
	debug_cols = {"Const", "S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8", "S9", "S10", "S11", "S12" };
	// println("%c",debug_cols, "%r", debug_cols, mT);
 
	// Construcao do mBeta4_tilda = mBeta4 + matriz Identidade
	mBeta4_tilda = mBeta4 + unit(rows(mBeta4));
	//	println(mBeta4_tilda);

	// Construcao da matrix identidade de Delta Y_{t-1} 
 	mI_1 = unit(rows(mBeta1));

	//Matrix = mBeta1 ~ mBeta2 ~ mBeta4 ~ mBeta3_tilda;
	Matrix = mBeta1 ~ mBeta2 ~ mBeta4;
	// println("Matrix", Matrix);
	
	//Matrix = Matrix | mI_1 ~ zeros(rows(mI_1), columns(mBeta2)) ~ zeros(rows(mI_1), columns(mBeta4)) ~ zeros(rows(mI_1), columns(mBeta3_tilda));
	Matrix = Matrix | mI_1 ~ zeros(rows(mI_1), columns(mBeta2)) ~ zeros(rows(mI_1), columns(mBeta4));
	// println("Matrix", Matrix);

	//Matrix = Matrix | mBeta1 ~ mBeta2 ~ mBeta4_tilda ~ mBeta3_tilda;
	Matrix = Matrix | mBeta1 ~ mBeta2 ~ mBeta4_tilda;
	// println("Matrix", Matrix);

	//Matrix = Matrix | zeros(rows(mT), columns(mBeta1)) ~ zeros(rows(mT), columns(mBeta2)) ~ zeros(rows(mT), columns(mBeta4)) ~ mT;
	
	//	println(Matrix);

	decl meval, mevec;

    //print(Matrix);
	savemat(sprint(txMatPathResult_Matrix, "Matrix_teste.mat"), Matrix);
	//print("result=", eigensym(Matrix, &meval, &mevec));

  	print("Beta 1 result=", eigen(mBeta1, &meval));
    print("Beta 1 eigenvalues:", "%r",{"real", "imaginary"}, meval);

	
	print("Full Matrix result=", eigen(Matrix, &meval));
    print("Full Matrix eigenvalues:", "%r",{"real", "imaginary"}, meval);
								
}
