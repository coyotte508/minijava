class A {
	Int x;
	Int y;

	Int distance() {
		x*x+y*y;
	}
}

class B extends A {
	Int distance2(A other) {
		A inter = new A();
		inter.x = other.x - x;
		inter.y = other.y - y;

		inter.distance();
	}
}