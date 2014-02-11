Bool good;
Bool bad;

Void print (String a) {
	print_string (a);
}

String result = 
	(
		if (good && !bad) {
			"Merry Christmas";
		} else {
			"No presents for you!";
		}
	);

print ("The result is: " + result);