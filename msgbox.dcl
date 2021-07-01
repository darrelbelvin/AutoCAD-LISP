lspOkCancel : dialog {

	key = "main";

	: column {

	: text {
	key = "message1";
	       }

	: text {
	key = "message2";
	       }

	: text {
	key = "message3";
	       }
	}
   	: row {

	: spacer { width = 1; }

	: button {
	label = "OK";
	key = "accept";
	width = 12;
	fixed_width = true;
	mnemonic = "O";
	is_default = true;
	}

	: button {
	label = "Cancel";
	key = "cancel";
	width = 12;
	fixed_width = true;
	mnemonic = "C";
	is_cancel = true;
  	}

	: spacer { width = 1;}

	}

  		 }

////////////////////////////////////////////////

lspYesNo : dialog {

	key = "main";

	: column {

	: text {
	key = "message1";
	       }

	: text {
	key = "message2";
	       }

	: text {
	key = "message3";
	       }
	}
   	: row {

	: spacer { width = 1; }

	: button {
	label = "Yes";
	key = "yes";
	width = 12;
	fixed_width = true;
	mnemonic = "Y";
	is_default = true;
	}

	: button {
	label = "No";
	key = "no";
	width = 12;
	fixed_width = true;
	mnemonic = "N";
	is_cancel = true;
  	}

	: spacer { width = 1;}

	}

  		 }

////////////////////////////////////////////

lspOkOnly : dialog {

	key = "main";

	: column {

	: text {
	key = "message1";
	       }

	: text {
	key = "message2";
	       }

	: text {
	key = "message3";
	       }
	}
   	: row {

	: spacer { width = 1; }

	: button {
	label = "OK";
	key = "accept";
	width = 12;
	fixed_width = true;
	mnemonic = "O";
	is_default = true;
	alignment = centered;
	}

	: spacer { width = 1;}

	}

  		 }

////////////////////////////////////////////////

lspYesNoCancel : dialog {

	key = "main";

	: column {

	: text {
	key = "message1";
	       }

	: text {
	key = "message2";
	       }

	: text {
	key = "message3";
	       }
	}
   	: row {

	: spacer { width = 1; }

	: button {
	label = "Yes";
	key = "yes";
	width = 12;
	fixed_width = true;
	mnemonic = "Y";
	is_default = true;
	}

	: button {
	label = "No";
	key = "no";
	width = 12;
	fixed_width = true;
	mnemonic = "N";
  	}

	: button {
	label = "Cancel";
	key = "cancel";
	width = 12;
	fixed_width = true;
	mnemonic = "C";
	is_cancel = true;
  	}

	: spacer { width = 1;}

	}

  		 }

////////////////////////////////////////////

lspRentryCancel : dialog {

	key = "main";

	: column {

	: text {
	key = "message1";
	       }

	: text {
	key = "message2";
	       }

	: text {
	key = "message3";
	       }
	}
   	: row {

	: spacer { width = 1; }

	: button {
	label = "Rentry";
	key = "rentry";
	width = 12;
	fixed_width = true;
	mnemonic = "R";
	is_default = true;
	}

	: button {
	label = "Cancel";
	key = "Cancel";
	width = 12;
	fixed_width = true;
	mnemonic = "C";
	is_cancel = true;
  	}

	: spacer { width = 1;}

	}

  		 }

////////////////////////////////////////////