/* Calls isFamily(b) instead of this.isFamily(b) */
class A {
	Int x;
	Int y;

	Int distance() {
		this.x*this.x+this.y*this.y;
	}
}

Void print(String x) {
	
}

class B extends A {
	Int distance2(A other) {
		A inter = new A();
		inter.x = other.x - this.x;
		inter.y = other.y - this.y;

		inter.distance();
	}

	Bool isFamily(Object b) {
		if (b == null) {
			false;
		} else  {
			b instanceof A;
		}
	}

	Bool greeting(Object b) {
		if (isFamily(b)) {
			Int distance = (A)b.distance();
			if (distance > 10) {
				print("hello, oh so far away..." );
			} else {
				print("hi there");
			}
			true;
		} else {
			false;
		}
	}
}