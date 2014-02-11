class A {
	Int x;
	Int y;

	Int distance() {
		this.x*this.x+this.y*this.y;
	}

	Void move(Int x, Int y) {
		this.x = x;
		this.y = y; 
	}
}

Void print(String s) {
	print_string (s); /* Built in */
}

print("I can print!");
print("");

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
		if (this.isFamily(b)) {
			Int distance = (A)b.distance();
			if (distance > 10) {
				print("Oh so far away..." );
			} else {
				print("hi there " + '"?"');
			}
			true;
		} else {
			print("I don't know you!");
			false;
		}
	}
}

B b1 = new B();
A b2 = new B();
A a1 = new A();

b2.move(3,4);

print ("Distance au carre entre b1 et b2: "); print_int (b1.distance2(b2));
b1.greeting(b2);

a1.move(2,2);

print ("");
print ("Distance au carre entre b1 et a1: "); print_int (b1.distance2(a1));
b1.greeting(a1);

print ("");

/* Void/Unit type */
b1.greeting( () );