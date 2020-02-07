%{

//#define YYSTYPE int
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <cstring>

using namespace std;

#define VARIABLE 0
#define ARRAY 1
#define CONSTANT 2
#define FORVARIABLE 3

#define NO_JUMP -1
#define I_JUMP 0
#define I_JZERO 1
#define I_JPOS 2
#define I_JNEG 3

extern int yylex();
extern int yyparse();
extern FILE* yyin;
extern int lineno;

ofstream outFile;
void yyerror(const char* s);

// memory before this is taken
// #define YY_DECL int yylex()
long firstfreemem = 1;

// variables in memory
typedef struct inmemvar {
	int type;
	string name;
	long value;
	long start;
	long end;
	long starttab;
	long endtab;
	bool initialized;
	// constans
	inmemvar(int type,long start,long value)
	{
		this->type = type;
		this->name = "";
		this->start = start;
		this->value = value;
	}
	// variable or forvariable
	inmemvar(int type,const char* name,long start)
	{
		this->type = type;
		this->name = name;
		this->start = start;
		if (type == FORVARIABLE)
		{
			this->initialized = true;
		}
		else
		{
			this->initialized = false;
		}
	}
	// array
	inmemvar(int type,const char* name,long start, long end, long starttab, long endtab)
	{
		this->type = type;
		this->name = name;
		this->start = start;
		this->end = end;
		this->starttab = starttab;
		this->endtab = endtab;
	}
} inmemvar;

long codestart = 0;
long nextdeclaration = 0;
long currline = 0;

long currarray = 3; // range 3-5

// virtual machine commands
typedef struct vmcommand {
	string comm;
	long reff;
	long jump; // reffer where to jump (+2 jump two forwards)
	long* memplace; // pointer to set memory place later
	long shift;
	vmcommand(string comm,long reff) // not jump constructor
	{
		this->comm = comm;
		this->memplace = nullptr;
		this->reff = reff;
		this->jump = 0;
		this->shift = 0;
	}
	vmcommand(string comm,long* memplace,long shift) // not jump constructor
	{
		this->comm = comm;
		this->memplace = memplace;
		this->reff = -1;
		this->jump = 0;
		this->shift = shift;
	}
	vmcommand(long jump,string comm) // jump constructor
	{
		this->comm = comm;
		this->memplace = nullptr;
		this->reff = -1;
		this->jump = jump;
		this->shift = 0;
	}
	vmcommand(string comm) // no arg constructor
	{
		this->comm = comm;
		this->memplace = nullptr;
		this->reff = -1;
		this->jump = 0;
		this->shift = 0;
	}
} vmcommand;

typedef struct Info {
	long codelength;
	long adress;
	bool isarray;
	int jumpkind; // what jump to perform (the other will be ordinary JUMP)
	bool ispositive; // is this jump for positive result (true) or negative (false;)
	bool isforvar;
	long val;
	Info(long codelength,long adress,bool isarray,int jumpkind,bool ispositive)
	{
		this->codelength = codelength;
		this->adress = adress;
		this->isarray = isarray;
		this->jumpkind = jumpkind;
		this->ispositive = ispositive;
		this->isforvar = false;
		this->val = 0;
	}
	Info(long codelength,long adress,bool isarray,int jumpkind,bool ispositive,long val)
	{
		this->codelength = codelength;
		this->adress = adress;
		this->isarray = isarray;
		this->jumpkind = jumpkind;
		this->ispositive = ispositive;
		this->isforvar = false;
		this->val = val;
	}
} Info;


vector <inmemvar> memory;
vector <vmcommand> code;

long wascreated(vector<inmemvar> &vect, long a);
long createnum(vector<inmemvar> &mem,vector<vmcommand> &cod,long a);
void searchdup(vector<inmemvar> &mem, string a);
long searchinmemory(vector<inmemvar> &mem, string a);
long searchinmemorytab(vector<inmemvar> &mem, string a,long b);
long searchinmemorytabshift(vector<inmemvar> &mem, string a);
int newmultiply(vector<vmcommand> &cod);
int newdivide(vector<vmcommand> &cod);
int newmodulo (vector<vmcommand> &cod);
void writeprogram(vector<vmcommand> &cod);
string DecToBin(long n);
void deleteID(long adress,vector<inmemvar> &mem);


%}

%union{
	char *idname;
	long value;
	struct Info* info; 
}

%token <value> num 
%token <idname> pidentifier
// nieterminale na dole maja wieksza wage czyli T_POWER > T_MULTIPLY > T_PLUS
%token <value> ASSIGN NLEFT NRIGHT DECLARE  BEGINN END SEMICOLON COLON COMMA IF THEN ELSE ENDIF WHILE DO ENDWHILE ENDDO FOR FROM TO DOWNTO ENDFOR READ WRITE
%token <value> EQ NEQ LE GE LEQ GEQ
%left PLUS MINUS
%left TIMES DIV MOD

%type <info> program declarations commands command expression condition value identifier foridentifier forendvalue

%start program

%%

program:      DECLARE declarations BEGINN commands END {code.push_back(vmcommand("HALT"));
	currline++;
	//cout<<lineno<<endl;
}
              | BEGINN commands END {code.push_back(vmcommand("HALT"));
	currline++;
	//cout<<lineno<<endl;
}

declarations: declarations COMMA pidentifier {
	$$ = 0;
	searchdup(memory,$3);
	memory.push_back(inmemvar(VARIABLE,$3,firstfreemem));
	firstfreemem++;
}
              | declarations COMMA pidentifier NLEFT num COLON num NRIGHT {
	$$ = 0;
	searchdup(memory,$3);
	long tabsize = $7 - $5;
	if (tabsize < 0)
	{
		// this is the way
		char er[50] = "Niewlasciwy zakres tablicy ";
		const char* err = strcat(er,$3);
		yyerror(err);
	}
	memory.push_back(inmemvar(ARRAY,$3,firstfreemem,firstfreemem+tabsize,$5,$7));
	firstfreemem += (tabsize + 1);
}
              | pidentifier {
	$$ = 0;
	searchdup(memory,$1);
	memory.push_back(inmemvar(VARIABLE,$1,firstfreemem));
	firstfreemem++;
}
              | pidentifier NLEFT num COLON num NRIGHT {
	$$ = 0;
	searchdup(memory,$1);
	long tabsize = $5 - $3;
	if (tabsize < 0)
	{
		char er[50] = "Niewlasciwy zakres tablicy ";
		const char* err = strcat(er,$1);
		yyerror(err);
	}
	memory.push_back(inmemvar(ARRAY,$1,firstfreemem,firstfreemem+tabsize,$3,$5));
	firstfreemem += (tabsize + 1);
}

commands:     commands command {
	$1->codelength = $1->codelength + $2->codelength;
	$$ = $1;
}
              | command {
	$$ = $1;
}

command:      identifier ASSIGN expression SEMICOLON {
	if ($1->isarray == true)
	{
		code.push_back(vmcommand("STOREI",$1->adress));
		currline+=1;
	}
	else
	{
		code.push_back(vmcommand("STORE",$1->adress));
		currline+=1;
	}

	$1->adress = 0;
	$1->codelength = $1->codelength + $3->codelength + 1;
	$1->isarray = false;
	$$ = $1;
}
              | IF condition THEN commands ELSE commands ENDIF {
	long mylen = $4->codelength;
	long mylen2 = $6->codelength;
	long l = 0;
	switch ($2->jumpkind)
	{
		case I_JZERO:
		{
			if ($2->ispositive == true)
			{
				if (static_cast<long>(code.size())-mylen-mylen2 < 0) // can go past 0
					yyerror("something went terribly wrong");
				code.insert(code.end()-mylen-mylen2,vmcommand(2,"JZERO"));
				code.insert(code.end()-mylen-mylen2,vmcommand(mylen+2,"JUMP"));
				code.insert(code.end()-mylen2,vmcommand(mylen2+1,"JUMP")); //ommit else
				l+=3;
			}
			else // if ($2->ispositive == false)
			{
				if (static_cast<long>(code.size())-mylen-mylen2 < 0)
					yyerror("something went terribly wrong");
				code.insert(code.end()-mylen-mylen2,vmcommand(mylen+2,"JZERO"));
				code.insert(code.end()-mylen2,vmcommand(mylen2+1,"JUMP")); //ommit else
				l+=2;
			}
			break;
		}
		case I_JNEG:
		{
			if ($2->ispositive == true)
			{
				if (static_cast<long>(code.size())-mylen-mylen2 < 0) // can go past 0
					yyerror("something went terribly wrong");
				code.insert(code.end()-mylen-mylen2,vmcommand(2,"JNEG"));
				code.insert(code.end()-mylen-mylen2,vmcommand(mylen+2,"JUMP"));
				code.insert(code.end()-mylen2,vmcommand(mylen2+1,"JUMP")); //ommit else
				l+=3;
			}
			else // if ($2->ispositive == false)
			{
				if (static_cast<long>(code.size())-mylen-mylen2 < 0)
					yyerror("something went terribly wrong");
				code.insert(code.end()-mylen-mylen2,vmcommand(mylen+2,"JNEG"));
				code.insert(code.end()-mylen2,vmcommand(mylen2+1,"JUMP")); //ommit else
				l+=2;
			}
			break;
		}
		case I_JPOS:
		{
			if ($2->ispositive == true)
			{
				if (static_cast<long>(code.size())-mylen-mylen2 < 0) // can go past 0
					yyerror("something went terribly wrong");
				code.insert(code.end()-mylen-mylen2,vmcommand(2,"JPOS"));
				code.insert(code.end()-mylen-mylen2,vmcommand(mylen+2,"JUMP"));
				code.insert(code.end()-mylen2,vmcommand(mylen2+1,"JUMP")); //ommit else
				l+=3;
			}
			else // if ($2->ispositive == false)
			{
				if (static_cast<long>(code.size())-mylen-mylen2 < 0)
					yyerror("something went terribly wrong");
				code.insert(code.end()-mylen-mylen2,vmcommand(mylen+2,"JPOS"));
				code.insert(code.end()-mylen2,vmcommand(mylen2+1,"JUMP")); //ommit else
				l+=2;
			}
			break;
		}
		default:
		{
			//cout<<$2->jumpkind<<endl;
			yyerror("Blad kompilatora");
		}
	}

	$2->adress = 0;
	$2->codelength = $2->codelength + $4->codelength + $6->codelength + l;
	$2->isarray = false;
	$$ = $2;
}
              | IF condition THEN commands ENDIF {
	long mylen = $4->codelength;
	long l = 0;
	switch ($2->jumpkind)
	{
		case I_JZERO:
		{
			if ($2->ispositive == true)
			{
				if (static_cast<long>(code.size())-mylen < 0) // can go past 0
					yyerror("something went terribly wrong");
				code.insert(code.end()-mylen,vmcommand(2,"JZERO"));
				code.insert(code.end()-mylen,vmcommand(mylen+1,"JUMP"));
				l+=2;
			}
			else // if ($2->ispositive == false)
			{
				if (static_cast<long>(code.size())-mylen < 0)
					yyerror("something went terribly wrong");
				code.insert(code.end()-mylen,vmcommand(mylen+1,"JZERO"));
				l+=1;
			}
			break;
		}
		case I_JNEG:
		{
			if ($2->ispositive == true)
			{
				if (static_cast<long>(code.size())-mylen < 0) // can go past 0
					yyerror("something went terribly wrong");
				code.insert(code.end()-mylen,vmcommand(2,"JNEG"));
				code.insert(code.end()-mylen,vmcommand(mylen+1,"JUMP"));
				l+=2;
			}
			else // if ($2->ispositive == false)
			{
				if (static_cast<long>(code.size())-mylen < 0)
					yyerror("something went terribly wrong");
				code.insert(code.end()-mylen,vmcommand(mylen+1,"JNEG"));
				l+=1;
			}
			break;
		}
		case I_JPOS:
		{
			if ($2->ispositive == true)
			{
				if (static_cast<long>(code.size())-mylen < 0) // can go past 0
					yyerror("something went terribly wrong");
				code.insert(code.end()-mylen,vmcommand(2,"JPOS"));
				code.insert(code.end()-mylen,vmcommand(mylen+1,"JUMP"));
				l+=2;
			}
			else // if ($2->ispositive == false)
			{
				if (static_cast<long>(code.size())-mylen < 0)
					yyerror("something went terribly wrong");
				code.insert(code.end()-mylen,vmcommand(mylen+1,"JPOS"));
				l+=1;
			}
			break;
		}
		default:
		{
			//cout<<$2->jumpkind<<endl;
			yyerror("Blad kompilatora");
		}
	}

	$2->adress = 0;
	$2->codelength = $2->codelength + $4->codelength + l;
	$2->isarray = false;
	$$ = $2;
}
              | WHILE condition DO commands ENDWHILE {
	long mylen = $4->codelength;
	long conlen = $2->codelength;
	long l = 0;
	switch ($2->jumpkind)
	{
		case I_JZERO:
		{
			if ($2->ispositive == true)
			{
				if (static_cast<long>(code.size())-mylen-conlen < 0) // can go past 0
					yyerror("something went terribly wrong");
				code.insert(code.end()-mylen,vmcommand(2,"JZERO"));
				code.insert(code.end()-mylen,vmcommand(mylen+2,"JUMP"));
				code.push_back(vmcommand(-mylen-2-conlen,"JUMP")); // jump to conditions
				l+=3;
			}
			else // if ($2->ispositive == false)
			{
				if (static_cast<long>(code.size())-mylen-conlen < 0)
					yyerror("something went terribly wrong");
				code.insert(code.end()-mylen,vmcommand(mylen+2,"JZERO"));
				code.push_back(vmcommand(-mylen-1-conlen,"JUMP")); // jump to conditions
				l+=2;
			}
			break;
		}
		case I_JNEG:
		{
			if ($2->ispositive == true)
			{
				if (static_cast<long>(code.size())-mylen-conlen < 0) // can go past 0
					yyerror("something went terribly wrong");
				code.insert(code.end()-mylen,vmcommand(2,"JNEG"));
				code.insert(code.end()-mylen,vmcommand(mylen+2,"JUMP"));
				code.push_back(vmcommand(-mylen-2-conlen,"JUMP")); // jump to conditions
				l+=3;
			}
			else // if ($2->ispositive == false)
			{
				if (static_cast<long>(code.size())-mylen-conlen < 0)
					yyerror("something went terribly wrong");
				code.insert(code.end()-mylen,vmcommand(mylen+2,"JNEG"));
				code.push_back(vmcommand(-mylen-1-conlen,"JUMP")); // jump to conditions
				l+=2;
			}
			break;
		}
		case I_JPOS:
		{
			if ($2->ispositive == true)
			{
				if (static_cast<long>(code.size())-mylen-conlen < 0) // can go past 0
					yyerror("something went terribly wrong");
				code.insert(code.end()-mylen,vmcommand(2,"JPOS"));
				code.insert(code.end()-mylen,vmcommand(mylen+2,"JUMP"));
				code.push_back(vmcommand(-mylen-2-conlen,"JUMP")); // jump to conditions
				l+=3;
			}
			else // if ($2->ispositive == false)
			{
				if (static_cast<long>(code.size())-mylen-conlen < 0)
					yyerror("something went terribly wrong");
				code.insert(code.end()-mylen,vmcommand(mylen+2,"JPOS"));
				code.push_back(vmcommand(-mylen-1-conlen,"JUMP")); // jump to conditions
				l+=2;
			}
			break;
		}
		default:
		{
			//cout<<$2->jumpkind<<endl;
			yyerror("Blad kompilatora");
		}
	}

	$2->adress = 0;
	$2->codelength = $2->codelength + $4->codelength + l;
	$2->isarray = false;
	$$ = $2;
}
              | DO commands WHILE condition ENDDO {
	long mylen = $2->codelength;
	long conlen = $4->codelength;
	long l = 0;
	switch ($4->jumpkind)
	{
		case I_JZERO:
		{
			if ($2->ispositive == true)
			{
				if (static_cast<long>(code.size())-mylen-conlen < 0) // can go past 0
					yyerror("something went terribly wrong");
				code.push_back(vmcommand(-mylen-conlen,"JZERO"));
				l+=1;
			}
			else // if ($2->ispositive == false)
			{
				if (static_cast<long>(code.size())-mylen-conlen < 0)
					yyerror("something went terribly wrong");
				code.push_back(vmcommand(2,"JZERO"));
				code.push_back(vmcommand(-mylen-1-conlen,"JUMP")); // jump to conditions
				l+=2;
			}
			break;
		}
		case I_JNEG:
		{
			if ($2->ispositive == true)
			{
				if (static_cast<long>(code.size())-mylen-conlen < 0) // can go past 0
					yyerror("something went terribly wrong");
				code.push_back(vmcommand(-mylen-conlen,"JNEG"));
				l+=1;
			}
			else // if ($2->ispositive == false)
			{
				if (static_cast<long>(code.size())-mylen-conlen < 0)
					yyerror("something went terribly wrong");
				code.push_back(vmcommand(2,"JNEG"));
				code.push_back(vmcommand(-mylen-1-conlen,"JUMP")); // jump to conditions
				l+=2;
			}
			break;
		}
		case I_JPOS:
		{
			if ($2->ispositive == true)
			{
				if (static_cast<long>(code.size())-mylen-conlen < 0) // can go past 0
					yyerror("something went terribly wrong");
				code.push_back(vmcommand(-mylen-conlen,"JPOS"));
				l+=1;
			}
			else // if ($2->ispositive == false)
			{
				if (static_cast<long>(code.size())-mylen-conlen < 0)
					yyerror("something went terribly wrong");
				code.push_back(vmcommand(2,"JPOS"));
				code.push_back(vmcommand(-mylen-1-conlen,"JUMP")); // jump to conditions
				l+=2;
			}
			break;
		}
		default:
		{
			//cout<<$4->jumpkind<<endl;
			yyerror("Blad kompilatora");
		}
	}

	$2->adress = 0;
	$2->codelength = $2->codelength + $4->codelength + l;
	$2->isarray = false;
	$$ = $2;
}
              | FOR foridentifier FROM value TO forendvalue DO commands ENDFOR {
	long mylen = $8->codelength;
	long len1 = $4->codelength;
	long len2 = $6->codelength;
	long l = 0;

	if (static_cast<long>(code.size())-mylen-len1-len2 < 0)
		yyerror("something went terribly wrong");
	if ($4->isarray == false)
	{
		code.insert(code.end()-mylen,vmcommand("LOAD",$4->adress)); // jedyna linijka roznicy w sumie
	}
	else // if ($4->isarray == true)
	{
		code.insert(code.end()-mylen,vmcommand("LOADI",$4->adress)); // jedyna linijka roznicy w sumie
	}
	code.insert(code.end()-mylen,vmcommand("STORE",$2->adress));
	code.insert(code.end()-mylen,vmcommand("LOAD",$6->adress));
	code.insert(code.end()-mylen,vmcommand("SUB",$2->adress));
	code.insert(code.end()-mylen,vmcommand(mylen+5,"JNEG"));
	code.push_back(vmcommand("LOAD",$2->adress));
	code.push_back(vmcommand("INC"));
	code.push_back(vmcommand("STORE",$2->adress));
	code.push_back(vmcommand(-mylen-6,"JUMP"));
	l = 9;

	// delete temporary IDs
	
	deleteID($2->adress,memory);

	$2->adress = 0;
	$2->codelength = $2->codelength + $4->codelength + $6->codelength + $8->codelength + l;
	$2->isarray = false;
	$$ = $2;
} 
              | FOR foridentifier FROM value DOWNTO forendvalue DO commands ENDFOR {
	long mylen = $8->codelength;
	long len1 = $4->codelength;
	long len2 = $6->codelength;
	long l = 0;

	if (static_cast<long>(code.size())-mylen-len1-len2 < 0)
		yyerror("something went terribly wrong");
	if ($4->isarray == false)
	{
		code.insert(code.end()-mylen,vmcommand("LOAD",$4->adress)); // jedyna linijka roznicy w sumie
	}
	else // if ($4->isarray == true)
	{
		code.insert(code.end()-mylen,vmcommand("LOADI",$4->adress)); // jedyna linijka roznicy w sumie
	}
	code.insert(code.end()-mylen,vmcommand("STORE",$2->adress));
	code.insert(code.end()-mylen,vmcommand("LOAD",$6->adress));
	code.insert(code.end()-mylen,vmcommand("SUB",$2->adress));
	code.insert(code.end()-mylen,vmcommand(mylen+5,"JPOS"));
	code.push_back(vmcommand("LOAD",$2->adress));
	code.push_back(vmcommand("DEC"));
	code.push_back(vmcommand("STORE",$2->adress));
	code.push_back(vmcommand(-mylen-6,"JUMP"));
	l = 9;

	// delete temporary IDs
	deleteID($2->adress,memory);
	/*
	memory[$6->adress].name = "";
	thrash.push_back($2->adress);
	thrash.push_back($6->adress);
	*/
	//////////

	$2->adress = 0;
	$2->codelength = $2->codelength + $4->codelength + $6->codelength + $8->codelength + l;
	$2->isarray = false;
	$$ = $2;
}
              | READ identifier SEMICOLON {
	// codelength,adress,isarray,jumpkind,ispositive
	if ($2->isarray == true)
	{
		code.push_back(vmcommand("GET"));
		code.push_back(vmcommand("STOREI",$2->adress));	
	}
	else
	{
		code.push_back(vmcommand("GET"));
		code.push_back(vmcommand("STORE",$2->adress));
	}
	currline+=2;

	$2->adress = 0; // not needed
	$2->codelength = $2->codelength + 2;
	$2->isarray = false;
	$$ = $2;
}
              | WRITE value SEMICOLON	{
	// codelength,adress,isarray,jumpkind,ispositive
	if ($2->isforvar == true)
	{
		yyerror("Nie mozna zmieniac wartosci zmiennych w petli FOR");
	}
	if ($2->isarray == true)
	{
		code.push_back(vmcommand("LOADI",$2->adress));
		code.push_back(vmcommand("PUT"));	
	}
	else
	{
		code.push_back(vmcommand("LOAD",$2->adress));
		code.push_back(vmcommand("PUT"));
	}
	currline+=2;

	$2->adress = 0; // not needed
	$2->codelength = $2->codelength + 2;
	$2->isarray = false;
	$$ = $2;
}

foridentifier: pidentifier {

	// stworz tymczasowy identyfikator
	searchdup(memory,$1);
	memory.push_back(inmemvar(FORVARIABLE,$1,firstfreemem));
	Info* newinfo = new Info(0,firstfreemem,false,-1,-1); // codelength,adress,isarray,jumpkind,ispositive
	newinfo->isforvar = true;
	firstfreemem++;
	$$ = newinfo;

}

forendvalue: value {
	// przechowuj koncowa wartosc dla fora
	if ($1->isarray == false)
	{
		code.push_back(vmcommand("LOAD",$1->adress)); // jedyna linijka roznicy
		code.push_back(vmcommand("STORE",firstfreemem));

		memory.push_back(inmemvar(FORVARIABLE,"",firstfreemem));
		$1->isforvar = true;
		$1->adress = firstfreemem;
		$1->codelength = $1->codelength + 2;
		$1->isarray = false;
		firstfreemem++;
		$$ = $1;
	}
	else // if ($1->isarray == true)
	{
		code.push_back(vmcommand("LOADI",$1->adress));
		code.push_back(vmcommand("STORE",firstfreemem));

		memory.push_back(inmemvar(FORVARIABLE,"",firstfreemem));
		$1->isforvar = true;
		$1->adress = firstfreemem;
		$1->codelength = $1->codelength + 2;
		$1->isarray = false;
		firstfreemem++;
		$$ = $1;
	}
}

expression:   value {
	if ($1->isarray == true)
	{
		code.push_back(vmcommand("LOADI",$1->adress));		
	}
	else
	{
		code.push_back(vmcommand("LOAD",$1->adress));	
	}
	currline+=1;

	$1->adress = 0; // result in p[0]
	$1->codelength = $1->codelength + 1;
	$1->isarray = false;
	$$ = $1;
}
              | value PLUS value {
	if ($1->isarray == false && $3->isarray == false)
	{
		code.push_back(vmcommand("LOAD",$1->adress));
		code.push_back(vmcommand("ADD",$3->adress));
		currline+=2;

		$1->adress = 0; // result in p[0]
		$1->codelength = $1->codelength + $3->codelength + 2;
		$1->isarray = false;
		$$ = $1;
	}
	else if ($1->isarray == true && $3->isarray == false)
	{
		code.push_back(vmcommand("LOADI",$1->adress));
		code.push_back(vmcommand("ADD",$3->adress));
		currline+=2;

		$1->adress = 0; // result in p[0]
		$1->codelength = $1->codelength + $3->codelength + 2;
		$1->isarray = false;
		$$ = $1;
	}
	else if ($1->isarray == false && $3->isarray == true)
	{
		code.push_back(vmcommand("LOADI",$3->adress));
		code.push_back(vmcommand("ADD",$1->adress));
		currline+=2;

		$1->adress = 0; // result in p[0]
		$1->codelength = $1->codelength + $3->codelength + 2;
		$1->isarray = false;
		$$ = $1;
	}
	else // if ($1->isarray == true && $3->isarray == true)
	{
		code.push_back(vmcommand("LOADI",$1->adress));
		code.push_back(vmcommand("STORE",&firstfreemem,0));
		code.push_back(vmcommand("LOADI",$3->adress));
		code.push_back(vmcommand("ADD",&firstfreemem,0));
		currline+=4;

		$1->adress = 0; // result in p[0]
		$1->codelength = $1->codelength + $3->codelength + 4;
		$1->isarray = false;
		$$ = $1;
	}
}
              | value MINUS value {
	if ($1->isarray == false && $3->isarray == false)
	{
		code.push_back(vmcommand("LOAD",$1->adress));
		code.push_back(vmcommand("SUB",$3->adress));
		currline+=2;

		$1->adress = 0; // result in p[0]
		$1->codelength = $1->codelength + $3->codelength + 2;
		$1->isarray = false;
		$$ = $1;
	}
	else if ($1->isarray == true && $3->isarray == false)
	{
		code.push_back(vmcommand("LOADI",$1->adress));
		code.push_back(vmcommand("SUB",$3->adress));
		currline+=2;

		$1->adress = 0; // result in p[0]
		$1->codelength = $1->codelength + $3->codelength + 2;
		$1->isarray = false;
		$$ = $1;
	}
	else if ($1->isarray == false && $3->isarray == true)
	{
		code.push_back(vmcommand("LOADI",$3->adress));
		code.push_back(vmcommand("STORE",&firstfreemem,0));
		code.push_back(vmcommand("LOAD",$1->adress));
		code.push_back(vmcommand("SUB",&firstfreemem,0));
		currline+=4;

		$1->adress = 0; // result in p[0]
		$1->codelength = $1->codelength + $3->codelength + 4;
		$1->isarray = false;
		$$ = $1;
	}
	else //if ($1->isarray == true && $3->isarray == true)
	{
		code.push_back(vmcommand("LOADI",$3->adress));
		code.push_back(vmcommand("STORE",&firstfreemem,0));
		code.push_back(vmcommand("LOADI",$1->adress));
		code.push_back(vmcommand("SUB",&firstfreemem,0));
		currline+=4;

		$1->adress = 0; // result in p[0]
		$1->codelength = $1->codelength + $3->codelength + 4;
		$1->isarray = false;
		$$ = $1;
	}
}
              | value TIMES value {
	int l = 0;
	if ($1->isarray == false && $3->isarray == false)
	{
		code.push_back(vmcommand("LOAD",$1->adress));
		code.push_back(vmcommand("STORE",&firstfreemem,4));
		code.push_back(vmcommand("LOAD",$3->adress));
		code.push_back(vmcommand("STORE",&firstfreemem,5));
		currline+=4;
		l += 4;
		if ($3->val == 2)
		{
			code.push_back(vmcommand("LOAD",&firstfreemem,4));
			code.push_back(vmcommand("SHIFT",1));
			l+=2;
		}
		else
		if ($3->val == -2)
		{
			code.push_back(vmcommand("LOAD",&firstfreemem,4));
			code.push_back(vmcommand("SUB",&firstfreemem,4));
			code.push_back(vmcommand("SUB",&firstfreemem,4));
			code.push_back(vmcommand("SHIFT",1));
			l+=4;
		}
		else
		{
			l += newmultiply(code);
		}
	}
	else if ($1->isarray == true && $3->isarray == false)
	{
		code.push_back(vmcommand("LOADI",$1->adress));
		code.push_back(vmcommand("STORE",&firstfreemem,4));
		code.push_back(vmcommand("LOAD",$3->adress));
		code.push_back(vmcommand("STORE",&firstfreemem,5));
		currline+=4;
		l += 4;
		if ($3->val == 2)
		{
			code.push_back(vmcommand("LOAD",&firstfreemem,4));
			code.push_back(vmcommand("SHIFT",1));
			l+=2;
		}
		else
		if ($3->val == -2)
		{
			code.push_back(vmcommand("LOAD",&firstfreemem,4));
			code.push_back(vmcommand("SUB",&firstfreemem,4));
			code.push_back(vmcommand("SUB",&firstfreemem,4));
			code.push_back(vmcommand("SHIFT",1));
			l+=4;
		}
		else
		{
			l += newmultiply(code);
		}
	}
	else if ($1->isarray == false && $3->isarray == true)
	{
		code.push_back(vmcommand("LOAD",$1->adress));
		code.push_back(vmcommand("STORE",&firstfreemem,4));
		code.push_back(vmcommand("LOADI",$3->adress));
		code.push_back(vmcommand("STORE",&firstfreemem,5));
		currline+=4;
		l += 4;
		if ($3->val == 2)
		{
			code.push_back(vmcommand("LOAD",&firstfreemem,4));
			code.push_back(vmcommand("SHIFT",1));
			l+=2;
		}
		else
		if ($3->val == -2)
		{
			code.push_back(vmcommand("LOAD",&firstfreemem,4));
			code.push_back(vmcommand("SUB",&firstfreemem,4));
			code.push_back(vmcommand("SUB",&firstfreemem,4));
			code.push_back(vmcommand("SHIFT",1));
			l+=4;
		}
		else
		{
			l += newmultiply(code);
		}
	}
	else //if ($1->isarray == true && $3->isarray == true)
	{
		code.push_back(vmcommand("LOADI",$1->adress));
		code.push_back(vmcommand("STORE",&firstfreemem,4));
		code.push_back(vmcommand("LOADI",$3->adress));
		code.push_back(vmcommand("STORE",&firstfreemem,5));
		currline+=4;
		l += 4;
		if ($3->val == 2)
		{
			code.push_back(vmcommand("LOAD",&firstfreemem,4));
			code.push_back(vmcommand("SHIFT",1));
			l+=2;
		}
		else
		if ($3->val == -2)
		{
			code.push_back(vmcommand("LOAD",&firstfreemem,4));
			code.push_back(vmcommand("SUB",&firstfreemem,4));
			code.push_back(vmcommand("SUB",&firstfreemem,4));
			code.push_back(vmcommand("SHIFT",1));
			l+=4;
		}
		else
		{
			l += newmultiply(code);
		}
	}

	$1->adress = 0; // result in p[0]
	$1->codelength = $1->codelength + $3->codelength + l;
	$1->isarray = false;
	$$ = $1;
}
              | value DIV value {
	int l = 0;
	if ($1->isarray == false && $3->isarray == false)
	{
		code.push_back(vmcommand("LOAD",$1->adress));
		code.push_back(vmcommand("STORE",&firstfreemem,4));
		code.push_back(vmcommand("LOAD",$3->adress));
		code.push_back(vmcommand("STORE",&firstfreemem,5));
		currline+=4;
		l += 4;
		if ($3->val == 2)
		{
			code.push_back(vmcommand("LOAD",&firstfreemem,4));
			code.push_back(vmcommand("SHIFT",2));
			l+=2;
		}
		else
		if ($3->val == -2)
		{
			code.push_back(vmcommand("LOAD",&firstfreemem,4));
			code.push_back(vmcommand("SUB",&firstfreemem,4));
			code.push_back(vmcommand("SUB",&firstfreemem,4));
			code.push_back(vmcommand("SHIFT",2));
			l+=4;
		}
		else
		{
			l += newdivide(code);
		}
	}
	else if ($1->isarray == true && $3->isarray == false)
	{
		code.push_back(vmcommand("LOADI",$1->adress));
		code.push_back(vmcommand("STORE",&firstfreemem,4));
		code.push_back(vmcommand("LOAD",$3->adress));
		code.push_back(vmcommand("STORE",&firstfreemem,5));
		currline+=4;
		l += 4;
		if ($3->val == 2)
		{
			code.push_back(vmcommand("LOAD",&firstfreemem,4));
			code.push_back(vmcommand("SHIFT",2));
			l+=2;
		}
		else
		if ($3->val == -2)
		{
			code.push_back(vmcommand("LOAD",&firstfreemem,4));
			code.push_back(vmcommand("SUB",&firstfreemem,4));
			code.push_back(vmcommand("SUB",&firstfreemem,4));
			code.push_back(vmcommand("SHIFT",2));
			l+=4;
		}
		else
		{
			l += newdivide(code);
		}
	}
	else if ($1->isarray == false && $3->isarray == true)
	{
		code.push_back(vmcommand("LOAD",$1->adress));
		code.push_back(vmcommand("STORE",&firstfreemem,4));
		code.push_back(vmcommand("LOADI",$3->adress));
		code.push_back(vmcommand("STORE",&firstfreemem,5));
		currline+=4;
		l += 4;
		if ($3->val == 2)
		{
			code.push_back(vmcommand("LOAD",&firstfreemem,4));
			code.push_back(vmcommand("SHIFT",2));
			l+=2;
		}
		else
		if ($3->val == -2)
		{
			code.push_back(vmcommand("LOAD",&firstfreemem,4));
			code.push_back(vmcommand("SUB",&firstfreemem,4));
			code.push_back(vmcommand("SUB",&firstfreemem,4));
			code.push_back(vmcommand("SHIFT",2));
			l+=4;
		}
		else
		{
			l += newdivide(code);
		}
	}
	else //if ($1->isarray == true && $3->isarray == true)
	{
		code.push_back(vmcommand("LOADI",$1->adress));
		code.push_back(vmcommand("STORE",&firstfreemem,4));
		code.push_back(vmcommand("LOADI",$3->adress));
		code.push_back(vmcommand("STORE",&firstfreemem,5));
		currline+=4;
		l += 4;
		if ($3->val == 2)
		{
			code.push_back(vmcommand("LOAD",&firstfreemem,4));
			code.push_back(vmcommand("SHIFT",2));
			l+=2;
		}
		else
		if ($3->val == -2)
		{
			code.push_back(vmcommand("LOAD",&firstfreemem,4));
			code.push_back(vmcommand("SUB",&firstfreemem,4));
			code.push_back(vmcommand("SUB",&firstfreemem,4));
			code.push_back(vmcommand("SHIFT",2));
			l+=4;
		}
		else
		{
			l += newdivide(code);
		}
	}

	$1->adress = 0; // result in p[0]
	$1->codelength = $1->codelength + $3->codelength + l;
	$1->isarray = false;
	$$ = $1;
}
              | value MOD value {
	int l = 0;
	if ($1->isarray == false && $3->isarray == false)
	{
		code.push_back(vmcommand("LOAD",$1->adress));
		code.push_back(vmcommand("STORE",&firstfreemem,4));
		code.push_back(vmcommand("LOAD",$3->adress));
		code.push_back(vmcommand("STORE",&firstfreemem,5));
		currline+=4;
		l = newmodulo(code);
		l += 4;
	}
	else if ($1->isarray == true && $3->isarray == false)
	{
		code.push_back(vmcommand("LOADI",$1->adress));
		code.push_back(vmcommand("STORE",&firstfreemem,4));
		code.push_back(vmcommand("LOAD",$3->adress));
		code.push_back(vmcommand("STORE",&firstfreemem,5));
		currline+=4;
		l = newmodulo(code);
		l += 4;
	}
	else if ($1->isarray == false && $3->isarray == true)
	{
		code.push_back(vmcommand("LOAD",$1->adress));
		code.push_back(vmcommand("STORE",&firstfreemem,4));
		code.push_back(vmcommand("LOADI",$3->adress));
		code.push_back(vmcommand("STORE",&firstfreemem,5));
		currline+=4;
		l = newmodulo(code);
		l += 4;
	}
	else //if ($1->isarray == true && $3->isarray == true)
	{
		code.push_back(vmcommand("LOADI",$1->adress));
		code.push_back(vmcommand("STORE",&firstfreemem,4));
		code.push_back(vmcommand("LOADI",$3->adress));
		code.push_back(vmcommand("STORE",&firstfreemem,5));
		currline+=4;
		l = newmodulo(code);
		l += 4;
	}

	$1->adress = 0; // result in p[0]
	$1->codelength = $1->codelength + $3->codelength + l;
	$1->isarray = false;
	$$ = $1;
}

condition:    value EQ value {
	// LOAD
	// SUB
	// JZERO (a calling loop or if will enter it later)
	// JUMP (a calling loop or if will enter it later)
	// ..
	// commands
	// ..
	// JUMP (if it's a loop)
	long l=0;
	if ($1->isarray == false && $3->isarray == false)
	{
		code.push_back(vmcommand("LOAD",$1->adress));
		code.push_back(vmcommand("SUB",$3->adress));
		currline+=2;
		l+=2;
	}
	else if ($1->isarray == true && $3->isarray == false)
	{
		code.push_back(vmcommand("LOADI",$1->adress));
		code.push_back(vmcommand("SUB",$3->adress));
		currline+=2;
		l+=2;
	}
	else if ($1->isarray == false && $3->isarray == true)
	{
		code.push_back(vmcommand("LOADI",$3->adress));
		code.push_back(vmcommand("SUB",$1->adress));
		currline+=2;
		l+=2;
	}
	else // if ($1->isarray == true && $3->isarray == true)
	{
		code.push_back(vmcommand("LOADI",$1->adress));
		code.push_back(vmcommand("STORE",&firstfreemem,0));
		code.push_back(vmcommand("LOADI",$3->adress));
		code.push_back(vmcommand("SUB",&firstfreemem,0));
		currline+=4;
		l+=4;
	}

	$1->adress = 0; // result in p[0]
	$1->codelength = $1->codelength + $3->codelength + l;
	$1->isarray = false;
	$1->jumpkind = I_JZERO; // now loops know how to jump
	$1->ispositive = true; 
	$$ = $1;	
}
              | value NEQ value {
	long l=0;
	if ($1->isarray == false && $3->isarray == false)
	{
		code.push_back(vmcommand("LOAD",$1->adress));
		code.push_back(vmcommand("SUB",$3->adress));
		currline+=2;
		l+=2;
	}
	else if ($1->isarray == true && $3->isarray == false)
	{
		code.push_back(vmcommand("LOADI",$1->adress));
		code.push_back(vmcommand("SUB",$3->adress));
		currline+=2;
		l+=2;
	}
	else if ($1->isarray == false && $3->isarray == true)
	{
		code.push_back(vmcommand("LOADI",$3->adress));
		code.push_back(vmcommand("SUB",$1->adress));
		currline+=2;
		l+=2;
	}
	else // if ($1->isarray == true && $3->isarray == true)
	{
		code.push_back(vmcommand("LOADI",$1->adress));
		code.push_back(vmcommand("STORE",&firstfreemem,0));
		code.push_back(vmcommand("LOADI",$3->adress));
		code.push_back(vmcommand("SUB",&firstfreemem,0));
		currline+=4;
		l+=4;
	}

	$1->adress = 0; // result in p[0]
	$1->codelength = $1->codelength + $3->codelength + 2;
	$1->isarray = false;
	$1->jumpkind = I_JZERO; // now loops know how to jump
	$1->ispositive = false;
	$$ = $1;
}
              | value LE value {
	long l=0;
	if ($1->isarray == false && $3->isarray == false)
	{
		code.push_back(vmcommand("LOAD",$1->adress));
		code.push_back(vmcommand("SUB",$3->adress));
		currline+=2;
		l+=2;
	}
	else if ($1->isarray == true && $3->isarray == false)
	{
		code.push_back(vmcommand("LOADI",$1->adress));
		code.push_back(vmcommand("SUB",$3->adress));
		currline+=2;
		l+=2;
	}
	else if ($1->isarray == false && $3->isarray == true)
	{
		code.push_back(vmcommand("LOADI",$3->adress));
		code.push_back(vmcommand("STORE",&firstfreemem,0));
		code.push_back(vmcommand("LOADI",$1->adress));
		code.push_back(vmcommand("SUB",&firstfreemem,0));
		currline+=4;
		l+=4;
	}
	else // if ($1->isarray == true && $3->isarray == true)
	{
		code.push_back(vmcommand("LOADI",$3->adress));
		code.push_back(vmcommand("STORE",&firstfreemem,0));
		code.push_back(vmcommand("LOADI",$1->adress));
		code.push_back(vmcommand("SUB",&firstfreemem,0));
		currline+=4;
		l+=4;
	}

	$1->adress = 0; // result in p[0]
	$1->codelength = $1->codelength + $3->codelength + 2;
	$1->isarray = false;
	$1->jumpkind = I_JNEG; // now loops know how to jump
	$1->ispositive = true;
	$$ = $1;
}
              | value GE value {
	long l=0;
	if ($1->isarray == false && $3->isarray == false)
	{
		code.push_back(vmcommand("LOAD",$1->adress));
		code.push_back(vmcommand("SUB",$3->adress));
		currline+=2;
		l+=2;
	}
	else if ($1->isarray == true && $3->isarray == false)
	{
		code.push_back(vmcommand("LOADI",$1->adress));
		code.push_back(vmcommand("SUB",$3->adress));
		currline+=2;
		l+=2;
	}
	else if ($1->isarray == false && $3->isarray == true)
	{
		code.push_back(vmcommand("LOADI",$3->adress));
		code.push_back(vmcommand("STORE",&firstfreemem,0));
		code.push_back(vmcommand("LOADI",$1->adress));
		code.push_back(vmcommand("SUB",&firstfreemem,0));
		currline+=4;
		l+=4;
	}
	else // if ($1->isarray == true && $3->isarray == true)
	{
		code.push_back(vmcommand("LOADI",$3->adress));
		code.push_back(vmcommand("STORE",&firstfreemem,0));
		code.push_back(vmcommand("LOADI",$1->adress));
		code.push_back(vmcommand("SUB",&firstfreemem,0));
		currline+=4;
		l+=4;
	}

	$1->adress = 0; // result in p[0]
	$1->codelength = $1->codelength + $3->codelength + 2;
	$1->isarray = false;
	$1->jumpkind = I_JPOS; // now loops know how to jump
	$1->ispositive = true;
	$$ = $1;
}
              | value LEQ value {
	long l=0;
	if ($1->isarray == false && $3->isarray == false)
	{
		code.push_back(vmcommand("LOAD",$1->adress));
		code.push_back(vmcommand("SUB",$3->adress));
		currline+=2;
		l+=2;
	}
	else if ($1->isarray == true && $3->isarray == false)
	{
		code.push_back(vmcommand("LOADI",$1->adress));
		code.push_back(vmcommand("SUB",$3->adress));
		currline+=2;
		l+=2;
	}
	else if ($1->isarray == false && $3->isarray == true)
	{
		code.push_back(vmcommand("LOADI",$3->adress));
		code.push_back(vmcommand("STORE",&firstfreemem,0));
		code.push_back(vmcommand("LOADI",$1->adress));
		code.push_back(vmcommand("SUB",&firstfreemem,0));
		currline+=4;
		l+=4;
	}
	else // if ($1->isarray == true && $3->isarray == true)
	{
		code.push_back(vmcommand("LOADI",$3->adress));
		code.push_back(vmcommand("STORE",&firstfreemem,0));
		code.push_back(vmcommand("LOADI",$1->adress));
		code.push_back(vmcommand("SUB",&firstfreemem,0));
		currline+=4;
		l+=4;
	}

	$1->adress = 0; // result in p[0]
	$1->codelength = $1->codelength + $3->codelength + 2;
	$1->isarray = false;
	$1->jumpkind = I_JPOS; // now loops know how to jump
	$1->ispositive = false;
	$$ = $1;
}
              | value GEQ value {
	long l=0;
	if ($1->isarray == false && $3->isarray == false)
	{
		code.push_back(vmcommand("LOAD",$1->adress));
		code.push_back(vmcommand("SUB",$3->adress));
		currline+=2;
		l+=2;
	}
	else if ($1->isarray == true && $3->isarray == false)
	{
		code.push_back(vmcommand("LOADI",$1->adress));
		code.push_back(vmcommand("SUB",$3->adress));
		currline+=2;
		l+=2;
	}
	else if ($1->isarray == false && $3->isarray == true)
	{
		code.push_back(vmcommand("LOADI",$3->adress));
		code.push_back(vmcommand("STORE",&firstfreemem,0));
		code.push_back(vmcommand("LOADI",$1->adress));
		code.push_back(vmcommand("SUB",&firstfreemem,0));
		currline+=4;
		l+=4;
	}
	else // if ($1->isarray == true && $3->isarray == true)
	{
		code.push_back(vmcommand("LOADI",$3->adress));
		code.push_back(vmcommand("STORE",&firstfreemem,0));
		code.push_back(vmcommand("LOADI",$1->adress));
		code.push_back(vmcommand("SUB",&firstfreemem,0));
		currline+=4;
		l+=4;
	}

	$1->adress = 0; // result in p[0]
	$1->codelength = $1->codelength + $3->codelength + 2;
	$1->isarray = false;
	$1->jumpkind = I_JNEG; // now loops know how to jump
	$1->ispositive = false;
	$$ = $1;
}

value:        num	{
	long i = wascreated(memory,$1);
	Info* newinfo;
	if (i)
	{
		newinfo = new Info(0,i,false,-1,-1,$1); // codelength,adress,isarray,jumpgood,jumpbad
		$$ = newinfo;
	}
	else
	{
		i = createnum(memory,code,$1);
		newinfo = new Info(0,i,false,-1,-1,$1); // codelength,adress,isarray,jumpgood,jumpbad
		$$ = newinfo;
	}
}
              | identifier	{
	if ($1->adress == 0)
	{
		yyerror("identifier not declared");
	}
	else
	{
		$$ = $1;
	}
}

identifier:   pidentifier {
	Info* newinfo = new Info(0,searchinmemory(memory,$1),false,-1,-1); // codelength,adress,isarray,jumpgood,jumpbad
	$$ = newinfo;
}
              | pidentifier NLEFT pidentifier NRIGHT {
	long index = searchinmemory(memory,$3);
	long shift = searchinmemorytabshift(memory,$1);

	// create number to shift
	long i = wascreated(memory,shift);
	if (i)
	{
		;
	}
	else
	{
		i = createnum(memory,code,shift);
	}


	code.push_back(vmcommand("LOAD",index));
	code.push_back(vmcommand("ADD",i));
	currarray = (currarray - 2) % 3 + 3;
	code.push_back(vmcommand("STORE",currarray));
	currline += 3;

	Info* newinfo = new Info(3,currarray,true,-1,-1); // codelength,adress,isarray,jumpgood,jumpbad
	$$ = newinfo;
}
              | pidentifier NLEFT num NRIGHT {
	Info* newinfo = new Info(0,searchinmemorytab(memory,$1,$3),false,-1,-1); // codelength,adress,isarray,jumpgood,jumpbad
	$$ = newinfo;
}



%%

int main(int argc, char *argv[]) {
	yyin = fopen(argv[1], "r");
	if (!yyin) // no file
	{
		yyerror("nie znaleziono pliku");
	}
	
  	outFile.open(argv[2]);

	memory.push_back(inmemvar(CONSTANT,firstfreemem,1));
	firstfreemem+=2;
	code.push_back(vmcommand("SUB",0));
	code.push_back(vmcommand("INC"));
	code.push_back(vmcommand("STORE",firstfreemem-2));
	code.push_back(vmcommand("DEC"));
	code.push_back(vmcommand("DEC"));
	code.push_back(vmcommand("STORE",firstfreemem-1));
	codestart += 6;
	nextdeclaration += 6;
	currline += 6;

	// elements 2,3,4 are reserved for arrays
	firstfreemem+=3;

	yyparse();

	writeprogram(code);

	outFile.close();
	for(inmemvar n : memory)
	{
        	//cout <<n.name<<" "<<n.start<<endl;
    	}
	return 0;
}

void yyerror(const char* s) {
	outFile.close();
	cout<<s<<" w linii "<<lineno<<endl;
	exit(1);
}

long wascreated(vector<inmemvar> &vect, long a)
{
	for(inmemvar n : vect)
	{
        	if (n.type == CONSTANT && n.value == a)
		{
			return n.start;
		}
    	}
	// not found
	return 0;
}
long createnum(vector<inmemvar> &mem,vector<vmcommand> &cod,long a)
{
	vector <vmcommand> newcode;
	if (a==0)
	{
		newcode.push_back(vmcommand("SUB",0));
		newcode.push_back(vmcommand("STORE",firstfreemem));

		mem.push_back(inmemvar(CONSTANT,firstfreemem,a));

		firstfreemem++;
		cod.insert(cod.begin() + nextdeclaration,newcode.begin(),newcode.end());
		long offset = newcode.size();
		codestart += offset;
		nextdeclaration += offset;
		currline += offset;

		return firstfreemem-1;
	}
	if (a<0)
	{
		newcode.push_back(vmcommand("SUB",0));
		string binnum = DecToBin(a);
		for (int j=0;j<binnum.length();j++)
		{
			if (binnum[j]=='0')
			{
				newcode.push_back(vmcommand("SHIFT",1));
			}
			else if (binnum[j]=='1')
			{
				newcode.push_back(vmcommand("SHIFT",1));
				newcode.push_back(vmcommand("DEC"));
			}
		}
		// store
		newcode.push_back(vmcommand("STORE",firstfreemem));
		// to memory
		mem.push_back(inmemvar(CONSTANT,firstfreemem,a));
		firstfreemem++;
		// store code
		cod.insert(cod.begin() + nextdeclaration,newcode.begin(),newcode.end());
		long offset = newcode.size();
		codestart += offset;
		nextdeclaration += offset;
		currline += offset;

		return firstfreemem-1;
	}
	else
	{
		newcode.push_back(vmcommand("SUB",0));
		string binnum = DecToBin(a);
		for (int j=0;j<binnum.length();j++)
		{
			if (binnum[j]=='0')
			{
				newcode.push_back(vmcommand("SHIFT",1));
			}
			else if (binnum[j]=='1')
			{
				newcode.push_back(vmcommand("SHIFT",1));
				newcode.push_back(vmcommand("INC"));
			}
		}
		newcode.push_back(vmcommand("STORE",firstfreemem));
		mem.push_back(inmemvar(CONSTANT,firstfreemem,a));
		firstfreemem++;
		cod.insert(cod.begin() + nextdeclaration,newcode.begin(),newcode.end());
		long offset = newcode.size();
		codestart += offset;
		nextdeclaration += offset;
		currline += offset;

		return firstfreemem-1;
	}
	return 0;
}
void searchdup(vector<inmemvar> &mem, string a)
{
	for (inmemvar i : mem)
	{
		if (i.name == a)
		{
			// this is the way
			char er[50] = "Druga deklaracja ";
			const char* s = a.c_str();
			const char* err = strcat(er,s);
			yyerror(err);
		}
	}
}

long searchinmemory(vector<inmemvar> &mem, string a)
{
	for (inmemvar i : mem)
	{
		if (i.name == a)
		{
			if (i.type == ARRAY)
			{
				// tried to access array as an ordinary variable
				// this is the way
				char er[50] = "Bledne odwolanie do zmiennej tablicowej ";
				const char* s = a.c_str();
				const char* err = strcat(er,s);
				yyerror(err);
				return 0;			
			}
			return i.start;
		}
	}
	// not found
	// this is the way
	char er[50] = "Niezadeklarowana zmienna ";
	const char* s = a.c_str();
	const char* err = strcat(er,s);
	yyerror(err);
	return 0;
}
long searchinmemorytab(vector<inmemvar> &mem, string a,long b)
{
	for (inmemvar i : mem)
	{
		if (i.name == a)
		{
			if (i.type == ARRAY)
			{
				if ( i.starttab <= b && b <= i.endtab)
				{
					return i.start + (b-i.starttab);
				}
				else
				{
					// tried to access array as an ordinary variable
					// this is the way
					char er[80] = "Odwolanie sie do elementu tablicy wykraczajacej poza zasieg tablicy  ";
					const char* s = a.c_str();
					const char* err = strcat(er,s);
					yyerror(err);
					return 0;
				}
			}
			else
			{
				// tried to access variable as an array
				// this is the way
				char er[50] = "Bledne odwolanie do zmiennej ";
				const char* s = a.c_str();
				const char* err = strcat(er,s);
				yyerror(err);
				return 0;
			}
		}
	}
	// not found
	char er[50] = "Niezadeklarowana zmienna tablicowa ";
	const char* s = a.c_str();
	const char* err = strcat(er,s);
	yyerror(err);
	return 0;
}
long searchinmemorytabshift(vector<inmemvar> &mem, string a)
{
	for (inmemvar i : mem)
	{
		if (i.name == a)
		{
			if (i.type == ARRAY)
			{
				return i.start -i.starttab;
			}
			else
			{
				// tried to access variable as an array
				// this is the way
				char er[50] = "Bledne odwolanie do zmiennej ";
				const char* s = a.c_str();
				const char* err = strcat(er,s);
				yyerror(err);
				return 0;
			}
		}
	}
	// not found
	yyerror("no such identifier");
	char er[50] = "Nizadeklarowana zmienna tablicowa ";
	const char* s = a.c_str();
	const char* err = strcat(er,s);
	yyerror(err);
	return 0;
}

void writeprogram(vector<vmcommand> &cod)
{
	for (int i=0; i < cod.size(); i++)
	{
        	outFile<<cod[i].comm<<" ";
		if (cod[i].reff != -1)
		{
			outFile<<cod[i].reff<<endl;
		}
		else
		{
			if (cod[i].jump != 0)
			{
				outFile<<i+cod[i].jump<<endl;
			}
			else
			{
				if (cod[i].memplace != nullptr)
				{
					outFile<<*(cod[i].memplace)+cod[i].shift<<endl;
				}
				else
				{
					outFile<<endl;
				}
			}
		}
    	}
}
string DecToBin(long n)
{
	if (n==0) return "0";
	if (n==1) return "1";
	if (n==-1) return "1";
	if (n%2 == 0)
	{
		return DecToBin(n/2) + "0";
	}
	else
	{
		return DecToBin(n/2) + "1";
	}
}
void deleteID(long adress,vector<inmemvar> &mem)
{
	for (long i=0;i<mem.size();i++)
	{
		if (mem[i].start == adress)
		{
			//cout<<mem[i].start<<" "<<mem[i].name<<endl;
			mem[i].name = "";
			return;
		}
	}
}
int newmultiply (vector<vmcommand> &cod)
{
	vector <vmcommand> newcode;
	// b * c
	// 1 = 1
	// 2 = -1
	// firstfreemem = b'
	// firstfreemem + 1 = c'
	// firstfreemem + 3 = result
	// firstfreemem + 4 = b // already loaded!
	// firstfreemem + 5 = c // already loaded!


	// clear
	newcode.push_back(vmcommand("SUB",0));
	newcode.push_back(vmcommand("STORE",&firstfreemem,3));
	// is b == 0
	newcode.push_back(vmcommand("LOAD",&firstfreemem,4));
	newcode.push_back(vmcommand(39,"JZERO")); // TO THE END
	newcode.push_back(vmcommand("STORE",&firstfreemem,0));
	// is c == 0
	newcode.push_back(vmcommand("LOAD",&firstfreemem,5));
	newcode.push_back(vmcommand(36,"JZERO")); // TO THE END
	newcode.push_back(vmcommand("STORE",&firstfreemem,1));
	newcode.push_back(vmcommand("LOAD",&firstfreemem,0));
	newcode.push_back(vmcommand(8,"JPOS")); // b is positive
	// if b<0 -> b*=-1 c*=-1
	newcode.push_back(vmcommand("SUB",&firstfreemem,0));
	newcode.push_back(vmcommand("SUB",&firstfreemem,0)); // p(0) = -b
	newcode.push_back(vmcommand("STORE",&firstfreemem,0));
	newcode.push_back(vmcommand("LOAD",&firstfreemem,1));
	newcode.push_back(vmcommand("SUB",&firstfreemem,1));
	newcode.push_back(vmcommand("SUB",&firstfreemem,1));
	newcode.push_back(vmcommand("STORE",&firstfreemem,1));


	//////////////////////////////////////// new code

	newcode.push_back(vmcommand("LOAD",&firstfreemem,0));
	newcode.push_back(vmcommand("DEC"));
	newcode.push_back(vmcommand(20,"JZERO")); // TO THE END
	newcode.push_back(vmcommand("INC"));
	newcode.push_back(vmcommand("SHIFT",2)); // *=-2
	newcode.push_back(vmcommand("SHIFT",1)); // *=2
	newcode.push_back(vmcommand("SUB",&firstfreemem,0));
	newcode.push_back(vmcommand(8,"JZERO")); // is even

	// odd
	newcode.push_back(vmcommand("LOAD",&firstfreemem,0));
	newcode.push_back(vmcommand("DEC"));
	newcode.push_back(vmcommand("STORE",&firstfreemem,0));
	newcode.push_back(vmcommand("LOAD",&firstfreemem,3));
	newcode.push_back(vmcommand("ADD",&firstfreemem,1));
	newcode.push_back(vmcommand("STORE",&firstfreemem,3));
	newcode.push_back(vmcommand(-14,"JUMP"));

	// even
	newcode.push_back(vmcommand("LOAD",&firstfreemem,0));
	newcode.push_back(vmcommand("SHIFT",2)); // a'*=2
	newcode.push_back(vmcommand("STORE",&firstfreemem,0));
	newcode.push_back(vmcommand("LOAD",&firstfreemem,1));
	newcode.push_back(vmcommand("SHIFT",1)); // b'/=2
	newcode.push_back(vmcommand("STORE",&firstfreemem,1));
	newcode.push_back(vmcommand(-21,"JUMP"));

	newcode.push_back(vmcommand("LOAD",&firstfreemem,1));
	newcode.push_back(vmcommand("ADD",&firstfreemem,3));
	newcode.push_back(vmcommand("STORE",&firstfreemem,3));
	newcode.push_back(vmcommand("LOAD",&firstfreemem,3));


	cod.insert(cod.end(),newcode.begin(),newcode.end());
	long offset = newcode.size();
	currline += offset;

	return 43; //how many lines were entered
}
int newdivide (vector<vmcommand> &cod)
{
	vector <vmcommand> newcode;
	// a / b
	// 1 = 1
	// 2 = -1
	// firstfreemem = a'
	// firstfreemem + 1 = b'
	// firstfreemem + 2 = div
	// firstfreemem + 3 = mod
	// firstfreemem + 4 = a // already loaded!
	// firstfreemem + 5 = b // already loaded!
	// firstfreemem + 6 = loop
	// firstfreemem + 7 = bits

	// clear
	newcode.push_back(vmcommand("SUB",0));
	newcode.push_back(vmcommand("STORE",&firstfreemem,2));
	newcode.push_back(vmcommand("STORE",&firstfreemem,3));
	newcode.push_back(vmcommand("INC"));
	newcode.push_back(vmcommand("STORE",&firstfreemem,7));
	// is b == 0 and b>0
	newcode.push_back(vmcommand("LOAD",&firstfreemem,4));
	newcode.push_back(vmcommand(82,"JZERO")); // TO THE END
	newcode.push_back(vmcommand(3,"JPOS")); // 
	newcode.push_back(vmcommand("SUB",&firstfreemem,4));
	newcode.push_back(vmcommand("SUB",&firstfreemem,4));
	newcode.push_back(vmcommand("STORE",&firstfreemem,0));
	// is c == 0 and c>0
	newcode.push_back(vmcommand("LOAD",&firstfreemem,5));
	newcode.push_back(vmcommand(76,"JZERO")); // TO THE END
	newcode.push_back(vmcommand(3,"JPOS")); // 
	newcode.push_back(vmcommand("SUB",&firstfreemem,5));
	newcode.push_back(vmcommand("SUB",&firstfreemem,5));
	newcode.push_back(vmcommand("STORE",&firstfreemem,1));

	newcode.push_back(vmcommand("STORE",&firstfreemem,6));
	newcode.push_back(vmcommand("SUB",&firstfreemem,0));
	newcode.push_back(vmcommand(2,"JPOS"));
	newcode.push_back(vmcommand(4,"JUMP")); 
	newcode.push_back(vmcommand("LOAD",&firstfreemem,0));
	newcode.push_back(vmcommand("STORE",&firstfreemem,3));
	newcode.push_back(vmcommand(38,"JUMP")); // TO THE END

	newcode.push_back(vmcommand("LOAD",&firstfreemem,6));
	newcode.push_back(vmcommand("SUB",&firstfreemem,0));
	newcode.push_back(vmcommand(8,"JPOS"));
	newcode.push_back(vmcommand("LOAD",&firstfreemem,6));
	newcode.push_back(vmcommand("SHIFT",1));
	newcode.push_back(vmcommand("STORE",&firstfreemem,6));
	newcode.push_back(vmcommand("LOAD",&firstfreemem,7));
	newcode.push_back(vmcommand("SHIFT",1));
	newcode.push_back(vmcommand("STORE",&firstfreemem,7));
	newcode.push_back(vmcommand(-9,"JUMP"));

	newcode.push_back(vmcommand("LOAD",&firstfreemem,6));
	newcode.push_back(vmcommand("SHIFT",2));
	newcode.push_back(vmcommand("STORE",&firstfreemem,6));
	newcode.push_back(vmcommand("LOAD",&firstfreemem,7));
	newcode.push_back(vmcommand("SHIFT",2));
	newcode.push_back(vmcommand("STORE",&firstfreemem,7));

	newcode.push_back(vmcommand("LOAD",&firstfreemem,0));
	newcode.push_back(vmcommand("STORE",&firstfreemem,3));
	// main loop
	newcode.push_back(vmcommand("LOAD",&firstfreemem,6));
	newcode.push_back(vmcommand("SUB",&firstfreemem,3));
	newcode.push_back(vmcommand(7,"JPOS"));
	newcode.push_back(vmcommand("LOAD",&firstfreemem,3));
	newcode.push_back(vmcommand("SUB",&firstfreemem,6));
	newcode.push_back(vmcommand("STORE",&firstfreemem,3));
	newcode.push_back(vmcommand("LOAD",&firstfreemem,2));
	newcode.push_back(vmcommand("ADD",&firstfreemem,7));
	newcode.push_back(vmcommand("STORE",&firstfreemem,2));
	newcode.push_back(vmcommand("LOAD",&firstfreemem,7));
	newcode.push_back(vmcommand("DEC"));
	newcode.push_back(vmcommand(8,"JZERO")); // TO THE END
	newcode.push_back(vmcommand("LOAD",&firstfreemem,6));
	newcode.push_back(vmcommand("SHIFT",2));
	newcode.push_back(vmcommand("STORE",&firstfreemem,6));
	newcode.push_back(vmcommand("LOAD",&firstfreemem,7));
	newcode.push_back(vmcommand("SHIFT",2));
	newcode.push_back(vmcommand("STORE",&firstfreemem,7));
	newcode.push_back(vmcommand(-18,"JUMP"));

	// almost end
	newcode.push_back(vmcommand("LOAD",&firstfreemem,4));
	newcode.push_back(vmcommand(14,"JNEG")); // a<0
	newcode.push_back(vmcommand("LOAD",&firstfreemem,5));
	newcode.push_back(vmcommand(2,"JNEG")); // b<0
	newcode.push_back(vmcommand(23,"JUMP")); // TO THE END
	newcode.push_back(vmcommand("LOAD",&firstfreemem,2));
	newcode.push_back(vmcommand("SUB",&firstfreemem,2));
	newcode.push_back(vmcommand("SUB",&firstfreemem,2));
	newcode.push_back(vmcommand("STORE",&firstfreemem,2));
	newcode.push_back(vmcommand("LOAD",&firstfreemem,3));
	newcode.push_back(vmcommand(17,"JZERO")); // TO THE END
	newcode.push_back(vmcommand("LOAD",&firstfreemem,2));
	newcode.push_back(vmcommand("DEC"));
	newcode.push_back(vmcommand("STORE",&firstfreemem,2));
	newcode.push_back(vmcommand(13,"JZERO")); // TO THE END

	newcode.push_back(vmcommand("LOAD",&firstfreemem,5));
	newcode.push_back(vmcommand(2,"JPOS")); // b<0
	newcode.push_back(vmcommand(10,"JUMP")); // TO THE END
	newcode.push_back(vmcommand("LOAD",&firstfreemem,2));
	newcode.push_back(vmcommand("SUB",&firstfreemem,2));
	newcode.push_back(vmcommand("SUB",&firstfreemem,2));
	newcode.push_back(vmcommand("STORE",&firstfreemem,2));
	newcode.push_back(vmcommand("LOAD",&firstfreemem,3));
	newcode.push_back(vmcommand(4,"JZERO")); // TO THE END
	newcode.push_back(vmcommand("LOAD",&firstfreemem,2));
	newcode.push_back(vmcommand("DEC"));
	newcode.push_back(vmcommand("STORE",&firstfreemem,2));
	// the end
	newcode.push_back(vmcommand("LOAD",&firstfreemem,2));



	cod.insert(cod.end(),newcode.begin(),newcode.end());
	long offset = newcode.size();
	currline += offset;

	return 89; //how many lines were entered
}
int newmodulo (vector<vmcommand> &cod)
{
	vector <vmcommand> newcode;
	// a / b
	// 1 = 1
	// 2 = -1
	// firstfreemem = a'
	// firstfreemem + 1 = b'
	// firstfreemem + 2 = div
	// firstfreemem + 3 = mod
	// firstfreemem + 4 = a // already loaded!
	// firstfreemem + 5 = b // already loaded!
	// firstfreemem + 6 = loop
	// firstfreemem + 7 = bits

	// clear
	newcode.push_back(vmcommand("SUB",0));
	newcode.push_back(vmcommand("STORE",&firstfreemem,2));
	newcode.push_back(vmcommand("STORE",&firstfreemem,3));
	newcode.push_back(vmcommand("INC"));
	newcode.push_back(vmcommand("STORE",&firstfreemem,7));
	// is b == 0 and b>0
	newcode.push_back(vmcommand("LOAD",&firstfreemem,4));
	newcode.push_back(vmcommand(79,"JZERO")); // TO THE END
	newcode.push_back(vmcommand(3,"JPOS")); // 
	newcode.push_back(vmcommand("SUB",&firstfreemem,4));
	newcode.push_back(vmcommand("SUB",&firstfreemem,4));
	newcode.push_back(vmcommand("STORE",&firstfreemem,0));
	// is c == 0 and c>0
	newcode.push_back(vmcommand("LOAD",&firstfreemem,5));
	newcode.push_back(vmcommand(73,"JZERO")); // TO THE END
	newcode.push_back(vmcommand(3,"JPOS")); // 
	newcode.push_back(vmcommand("SUB",&firstfreemem,5));
	newcode.push_back(vmcommand("SUB",&firstfreemem,5));
	newcode.push_back(vmcommand("STORE",&firstfreemem,1));

	newcode.push_back(vmcommand("STORE",&firstfreemem,6));
	newcode.push_back(vmcommand("SUB",&firstfreemem,0));
	newcode.push_back(vmcommand(2,"JPOS"));
	newcode.push_back(vmcommand(4,"JUMP")); 
	newcode.push_back(vmcommand("LOAD",&firstfreemem,0));
	newcode.push_back(vmcommand("STORE",&firstfreemem,3));
	newcode.push_back(vmcommand(38,"JUMP")); // TO THE END

	newcode.push_back(vmcommand("LOAD",&firstfreemem,6));
	newcode.push_back(vmcommand("SUB",&firstfreemem,0));
	newcode.push_back(vmcommand(8,"JPOS"));
	newcode.push_back(vmcommand("LOAD",&firstfreemem,6));
	newcode.push_back(vmcommand("SHIFT",1));
	newcode.push_back(vmcommand("STORE",&firstfreemem,6));
	newcode.push_back(vmcommand("LOAD",&firstfreemem,7));
	newcode.push_back(vmcommand("SHIFT",1));
	newcode.push_back(vmcommand("STORE",&firstfreemem,7));
	newcode.push_back(vmcommand(-9,"JUMP"));

	newcode.push_back(vmcommand("LOAD",&firstfreemem,6));
	newcode.push_back(vmcommand("SHIFT",2));
	newcode.push_back(vmcommand("STORE",&firstfreemem,6));
	newcode.push_back(vmcommand("LOAD",&firstfreemem,7));
	newcode.push_back(vmcommand("SHIFT",2));
	newcode.push_back(vmcommand("STORE",&firstfreemem,7));

	newcode.push_back(vmcommand("LOAD",&firstfreemem,0));
	newcode.push_back(vmcommand("STORE",&firstfreemem,3));
	// main loop
	newcode.push_back(vmcommand("LOAD",&firstfreemem,6));
	newcode.push_back(vmcommand("SUB",&firstfreemem,3));
	newcode.push_back(vmcommand(7,"JPOS"));
	newcode.push_back(vmcommand("LOAD",&firstfreemem,3));
	newcode.push_back(vmcommand("SUB",&firstfreemem,6));
	newcode.push_back(vmcommand("STORE",&firstfreemem,3));
	newcode.push_back(vmcommand("LOAD",&firstfreemem,2));
	newcode.push_back(vmcommand("ADD",&firstfreemem,7));
	newcode.push_back(vmcommand("STORE",&firstfreemem,2));
	newcode.push_back(vmcommand("LOAD",&firstfreemem,7));
	newcode.push_back(vmcommand("DEC"));
	newcode.push_back(vmcommand(8,"JZERO")); // TO THE END
	newcode.push_back(vmcommand("LOAD",&firstfreemem,6));
	newcode.push_back(vmcommand("SHIFT",2));
	newcode.push_back(vmcommand("STORE",&firstfreemem,6));
	newcode.push_back(vmcommand("LOAD",&firstfreemem,7));
	newcode.push_back(vmcommand("SHIFT",2));
	newcode.push_back(vmcommand("STORE",&firstfreemem,7));
	newcode.push_back(vmcommand(-18,"JUMP"));

	// almost end
	newcode.push_back(vmcommand("LOAD",&firstfreemem,3));//jak modulo=0 to nic nie zmieniamy
	newcode.push_back(vmcommand(23,"JZERO"));	

	newcode.push_back(vmcommand("LOAD",&firstfreemem,4));
	newcode.push_back(vmcommand(8,"JNEG")); // a<0
	newcode.push_back(vmcommand("LOAD",&firstfreemem,5));
	newcode.push_back(vmcommand(2,"JNEG")); // b<0
	newcode.push_back(vmcommand(18,"JUMP")); // TO THE END
	newcode.push_back(vmcommand("LOAD",&firstfreemem,3));
	newcode.push_back(vmcommand("ADD",&firstfreemem,5));
	newcode.push_back(vmcommand("STORE",&firstfreemem,3));
	newcode.push_back(vmcommand(14,"JUMP")); // TO THE END

	newcode.push_back(vmcommand("LOAD",&firstfreemem,5));
	newcode.push_back(vmcommand(6,"JPOS"));
	newcode.push_back(vmcommand("LOAD",&firstfreemem,3));
	newcode.push_back(vmcommand("SUB",&firstfreemem,3));
	newcode.push_back(vmcommand("SUB",&firstfreemem,3));
	newcode.push_back(vmcommand("STORE",&firstfreemem,3));
	newcode.push_back(vmcommand(7,"JUMP")); // TO THE END
	newcode.push_back(vmcommand("LOAD",&firstfreemem,3));
	newcode.push_back(vmcommand("SUB",&firstfreemem,5));
	newcode.push_back(vmcommand("STORE",&firstfreemem,3));
	newcode.push_back(vmcommand("SUB",&firstfreemem,3));
	newcode.push_back(vmcommand("SUB",&firstfreemem,3));
	newcode.push_back(vmcommand("STORE",&firstfreemem,3));
	// the end
	newcode.push_back(vmcommand("LOAD",&firstfreemem,3));



	cod.insert(cod.end(),newcode.begin(),newcode.end());
	long offset = newcode.size();
	currline += offset;

	return 86; //how many lines were entered
}
