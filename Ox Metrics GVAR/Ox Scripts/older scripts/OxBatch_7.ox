#include <oxstd.oxh>
#import <packages/PcGive/pcgive>

main()
{
	decl mX, i;

	mX = {"Adm","Des"};


	println("TAMANHO: ",sizeof(mX));
	println("VALOR: ", mX[0]);

	for(i=0; i < sizeof(mX); ++i)
	{
		println(mX[i]);
	}

}
