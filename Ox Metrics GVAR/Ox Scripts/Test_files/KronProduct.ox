#include <oxstd.oxh>

main()
{
	// enter code
	println("Hello world");

	// Declara as matrizes
	decl a, b, c;

	// Inicializa a matriz a como sendo uma 2x3
	a = <1, 2, 3; 5, 7, 9>;
	println(a);

	// Inicializa a matriz b como sendo uma 2x3
	b = <11,23>;
	println(b);

	c = <1, 0; 0, 0> ** b;

	c = a ~ zeros(rows(a), columns(b));
	a = (a ~ zeros(rows(a), columns(b))) | (zeros(rows(b), columns(a)) ~ b);
	b=b|b;
	a = (a ~ zeros(rows(a), columns(b))) | (zeros(rows(b), columns(a)) ~ b);
	
	println(a);
		 

}
