%{
#include <cstdio>
#include <iostream>
#include <map>
#include <fstream>
#include <vector>
#include <string>
#include <unordered_map>
#include <stack>
#include <sstream>

using namespace std;

// stuff from flex that bison needs to know about:
extern "C" int yylex();
extern "C" int yyparse();
extern "C" FILE *yyin;

void yyerror(const char *s);

extern map<string,string> ConstantsSymTab;

ofstream ofile,ofile1;


string s = "";
string function_name = "";
map<string, string> globalSymbolTable;

map<string,string> symbolTableBasicTypes;

typedef unordered_map<std::string,std::string> RecordsFields;
RecordsFields recordDetails;
map<string, RecordsFields> symbolTableRecordTypes;


struct ArrayMiscellaneos {
  string l;
  string u;
  string type;
};

string toReg;
string lower ="";
string higher="";
string typeOfArray; 
map<string,struct ArrayMiscellaneos> symbolTableArrayTypes;
void setLower();
void setHigher();

string arg1 ="";
string arg2 ="";
void insertType();
void insertRecordType();
void insertArrayType();
vector<string> arg3;
string arg4="";
void insertVar();


string global1;
string global2;
void insertGlobalSymbolTable();

vector<string> variableList;
string variableType="";
string argVariable="";
void insertVariable();


map<string,string> temporarySymbolTable;
vector<string> identifierList;
string typeofIdentifier;
void insertFunc();


string variableToCheck="";
void checkVariable();

string variableBeingAssigned="";
string variableAssignedTo="";
void checkTypeEquivalance();


string arg5="";
vector<string> arg6;
vector<string> arg8;
string arg7="";


map<string,string> symbolTableFunctions;
void insertIntoFunctionSymbolTable();

bool doNotPut =  true;

bool ifundeclared = false;

bool flagReturn = false;

////////////////

int TAC_temp_counter = 0;
string TAC_variable1 = "";
string TAC_variable2 = "";
string TAC_operator = "";
stack<string> TAC_add_counter;
stack<string> TAC_Addop_counter;
int TAC_flag = 0;
int TAC_minusflag = 0;
int TAC_notflag = 0;
int TAC_label_counter = 0;
int TAC_expression_total = 0;
int TAC_var1 = 0;
int TAC_var2 = 0;
string TAC_op = "";
bool singleFactor = true;
void processFactor();
string finalV1="";
string finalV2="";
string finalVop = "";
string finalVar ="";
int topLarger = 0;
vector<string> parameters;
string procedureName="";
string variableDot="";
string tempString="";
string tempOperator="";
int tempFlag=0;
bool comingFrom=true;


int labelCounter = 1;
string globalVariable;
string expression1,expression2;


stack<int> forLoops;
stack<int> whileLoops;
stack<int> ifLoops;
stack<int> forLoopsLabelCounter; 



string rel1,rel2,relOp;

%}

%union {
	char *sval;
}

// define the "terminal symbol" token types I'm going to use (in CAPS
// by convention), and associate each with a field of the union:
%token <sval> INT
%token <sval> ID

%token AND ARRAY ASSIGNMENT STRING COLON COMMA
%token DIV DO DOT DOTDOT ELSE END EQUAL FOR FORWARD FUNCTION
%token GE GT IF LBRAC LE LPAREN LT MINUS MOD NOT
%token NOTEQUAL OF OR PBEGIN PLUS PROCEDURE PROGRAM RBRAC
%token RECORD RPAREN SEMICOLON THEN 
%token TO TYPE VAR WHILE MULTIPLICATION
%nonassoc LOWER_THAN_ELSE
%nonassoc ELSE

%%
Program : PROGRAM ID SEMICOLON TypeDefinitions VariableDeclarations SubProgramDeclarations CompoundStatement DOT { ofile << "Program" << endl; 
          global2 = "Program" ; global1 = $2; insertGlobalSymbolTable();  }               					
        ;									

TypeDefinitions : TYPE TypeDefinitionList { ofile << "TypeDefinitions" << endl; }
                |
		;

TypeDefinitionList : TypeDefinition SEMICOLON 
                   | TypeDefinitionList TypeDefinition SEMICOLON
                   ;

TypeDefinition :  ID EQUAL Type { ofile << "TypeDefinition" << endl; arg1=$1 ; if(arg2.compare("Record")==0) insertRecordType();
                                  else if (arg2.compare("Array")==0) insertArrayType(); else insertType();
                }
               ;

Type : ID  { ofile << "Type" << endl; arg2=$1;   arg4=$1;  arg8.push_back($1); typeOfArray = $1; variableType = $1; typeofIdentifier = $1; }  
     | RECORD FieldList END  { ofile << "Type" << endl; arg2="Record";  arg4="Record"; arg8.push_back("RECORD"); }
     | ARRAY LBRAC Aux RBRAC OF Type { ofile << "Type" << endl; arg2="Array";  arg4="Array"; arg8.push_back("ARRAY"); }
     ;

Aux : Constant DOTDOT Constant  
    ;

VariableDeclarations : VAR VariableDeclarationList {  ofile << "VariableDeclarations" << endl; TAC_temp_counter = topLarger; }
                     |
		     ;	

VariableDeclarationList : VariableDeclaration SEMICOLON
			| VariableDeclarationList VariableDeclaration SEMICOLON
			;

VariableDeclaration : IdentifierList COLON Type  { ofile << "VariableDeclaration" << endl; if(doNotPut) insertVariable(); else  insertFunc(); }
	            ;


SubProgramDeclarations : SubProgramDeclarationLists { ofile << "SubProgramDeclarations" << endl; /* temporarySymbolTable.clear();*/  }
		       | 
		       ; 

SubProgramDeclarationListTypes : ProcedureDeclaration SEMICOLON
			       | FunctionDeclaration SEMICOLON 
			       ;
			   	
SubProgramDeclarationLists : SubProgramDeclarationListTypes 
		           | SubProgramDeclarationLists SubProgramDeclarationListTypes	
		           ; 


ProcedureDeclaration : PROCEDURE ID LPAREN FormalParameterList RPAREN SEMICOLON BlockOrForward { ofile << "ProcedureDeclaration" << endl; 
                        global1 = $2; global2 = "PROCEDURE"; insertGlobalSymbolTable(); if(flagReturn) ofile1 << "return" << endl; flagReturn = false;
			 }      
		     ;
			
FunctionDeclaration : FunctionHeading LPAREN FormalParameterList RPAREN COLON ResultType SEMICOLON BlockOrForward { ofile << "FunctionDeclaration" << endl;  ofile << "Exiting:" << endl;  temporarySymbolTable.clear(); variableBeingAssigned =""; /*doNotPut = false;*/ TAC_variable1="";
flagReturn = false;

 } 		   ;
FunctionHeading : FUNCTION ID  { global1 = $2; }
                ;

BlockOrForward : Block  { flagReturn = true;  } 
	       | FORWARD 
	       ;
					
FormalParameterList : FormalParameterListExist { ofile << "FormalParameterList" << endl;  }
		    |
		    ;

FormalParameterListExist : IdentifierList COLON Type {insertFunc();} 
			 | FormalParameterListExist SEMICOLON IdentifierList COLON Type {insertFunc();}
			 ;	 
			 	
			
Block : VariableDeclarations CompoundStatement { ofile << "Block" << endl; }
      ; 
      
  	           
CompoundStatement : PBEGIN StatementSequence END { ofile << "CompoundStatement" << endl;  ofile << "StatementSequence" << endl;  }
                  ;

StatementSequence : Statement {TAC_variable1=""; TAC_variable2="";  }
		  | StatementSequence SEMICOLON Statement { TAC_variable1 =""; TAC_variable2 =""; }
		  ;
		  		
 	  	    	  	  	  	  	
Statement : SimpleStatement { ofile << "Statement" << endl;  } 
          | StructuredStatement { ofile << "Statement" << endl; } 
	  ;
	         
SimpleStatement : SimpleStatementExist { ofile << "SimpleStatement" << endl; TAC_variable1=""; TAC_variable2=""; TAC_flag = 0; }
	        |		
		;
		      				
SimpleStatementExist : AssignmentStatement {TAC_variable1 =""; TAC_variable2 =""; TAC_flag = 1; parameters.clear(); } 
		     | ProcedureStatement 
		     ; 			
		     	
AssignmentStatement : VariableTemp ASSIGNMENT Expression { ofile << "AssignmentStatement" << endl;  if(!ifundeclared)checkTypeEquivalance(); ifundeclared = false; singleFactor = true; 

if(symbolTableFunctions.find(finalVar)!=symbolTableFunctions.end())
ofile1 << "funreturn " << finalV1 << endl;
else
ofile1 << finalVar <<  " := " <<  finalV1 << " " << finalVop << " " << finalV2 << endl;
finalV1 = "";
finalVop = "";
finalV2 = "";
TAC_variable1 = "";
TAC_variable2 = "";
finalVar="";
}	            ;



VariableTemp : Variable {  finalVar = TAC_variable1; parameters.clear(); TAC_variable1=""; comingFrom = true; }
	     ;


ProcedureStatement : ID LPAREN ActualParameterList RPAREN { ofile << "ProcedureStatement" << endl; variableToCheck = $1; checkVariable(); ifundeclared = false;
        ofile1 << "param ";						
       for(int i=0; i<parameters.size(); i++) 				
        ofile1 << parameters.at(i) << " ";					
       ofile1 << endl;  								
       ofile1 << "call " << $1 << endl;    parameters.clear(); 	            	        	                   
 }                   ;						



	
StructuredStatement : CompoundStatement { ofile << "StructuredStatement" << endl; }
		    | IfFirstPart Statement %prec LOWER_THAN_ELSE { ofile << "StructuredStatement" << endl;
		           ofile1 << "endL"<< (forLoopsLabelCounter.top()) << ":" << endl;
                           forLoopsLabelCounter.pop();
		    }
		    | IfFirstPart IfMiddlePart ELSE Statement { ofile << "StructuredStatement" << endl;
		     }
		    | WhileFirstPart DO Statement { ofile << "StructuredStatement" << endl; 
			   stringstream ss;
			   ss << labelCounter;
			   int temp = 0;
			   if(!whileLoops.empty())	
			   temp = whileLoops.top();
			   whileLoops.pop();
			   ofile1 << " if ( t" << (temp) << " ) goto L" << (forLoopsLabelCounter.top())	<< endl;
			   ofile1 << "endL"<< (forLoopsLabelCounter.top()) << ":" << endl;
			   forLoopsLabelCounter.pop();
			}
		    | FirstPartFor SecondPart DO Statement { 
				ofile << "StructuredStatement" << endl;
				ofile1 << globalVariable << " := " << globalVariable << " + 1 " << endl;   
				stringstream ss;
				ss << labelCounter;
				int temp = 0;
        	                if(!forLoops.empty())
	                        temp = forLoops.top();
			        ofile1 << " if ( " << globalVariable << " <= t" << temp << " ) goto L" << (forLoopsLabelCounter.top()) << endl;
				ofile1 << "endL"<< forLoopsLabelCounter.top() << ":" << endl;
				forLoopsLabelCounter.pop();
		     }   			
		    ; 
             
IfFirstPart : IF Expression { 
		   ofile1 << "t" << (TAC_temp_counter+1) << " := not t" << TAC_temp_counter << endl;
                   ofile1 << " if ( " << "t" << (TAC_temp_counter+1) << " ) goto endL" << labelCounter << endl;
                   TAC_temp_counter++;
                   stringstream ss;
                   ss << labelCounter;
                   ofile1 << "L" << labelCounter << ":" << endl;
                   forLoopsLabelCounter.push(labelCounter);
                   labelCounter++;
	    }
	    ;


IfMiddlePart : THEN Statement {
		           ofile1 << "endL"<< (forLoopsLabelCounter.top()) << ":" << endl;
                           forLoopsLabelCounter.pop();
				ofile1 << "else " << endl;
             }
	     ;	

WhileFirstPart : WHILE Expression {
		   ofile1 << "t" << (TAC_temp_counter+1) << " := not t" << TAC_temp_counter << endl;					
		   whileLoops.push(TAC_temp_counter);		
  		   ofile1 << " if ( " << "t" << (TAC_temp_counter+1) << " ) goto endL" << labelCounter << endl; 	 
                   TAC_temp_counter++;
		   stringstream ss;
		   ss << labelCounter;	  
		   ofile1 << "L" << labelCounter << ":" << endl;
		   forLoopsLabelCounter.push(labelCounter);
		   labelCounter++;	            	      
	         }				
	       ;


FirstPartFor : FOR ID ASSIGNMENT Expression {  
				stringstream ss,ss1;
				ss << TAC_temp_counter;
				expression1 = "t" + ss.str();
				globalVariable = $2;
				ofile1 << $2 << " = " << "t" + ss.str() << endl;			
                                }  
	     ; 

SecondPart : TO Expression {
			stringstream ss;
			ss << TAC_temp_counter;
			forLoops.push(TAC_temp_counter);
			expression2 = "t" + ss.str();
			ofile1 << "if ( " <<  globalVariable << " > " << "t" + ss.str() << " ) goto endL" << (labelCounter) <<  endl;
                        ofile1 << "L" << labelCounter << ":" << endl; 
			forLoopsLabelCounter.push(labelCounter);
			labelCounter++;
			}                  
	    ;



FieldList : IdentifierList COLON Type  { ofile << "FieldList" << endl; insertVar(); }
          | FieldList SEMICOLON IdentifierList COLON Type { insertVar(); } 
	  |
          ;
                                                                           
Constant : Sign INT { ofile << "Constant" << endl; toReg =  $2; if(lower == "") { setLower(); } else setHigher(); }
         ;
   
Sign : PLUS { ofile << "Sign" << endl; }
     | MINUS { ofile << "Sign" << endl; TAC_minusflag = 1; }
     |
     ;

ResultType : ID { ofile << "ResultType" << endl; global2 = $1; doNotPut = false; insertIntoFunctionSymbolTable(); }
	   ;

Expression : SimpleExpression { 
				int top;
				if(!TAC_add_counter.empty()) {
				istringstream bb(TAC_add_counter.top());
                                bb >> top; 
				top++;
                                } 
				                                
				if(comingFrom==false) {
				TAC_add_counter.pop();
				break;
				}
				while(!TAC_add_counter.empty()) {
			        string v1 = TAC_add_counter.top();  

                                TAC_add_counter.pop();  
				int tt; 
                                istringstream buffer(v1);  
                                buffer >> tt;
 				string v11 = "t" + v1; 
                                if(TAC_add_counter.empty()) {  finalV1 = v11;    break;  }
				
                                string v2 = TAC_add_counter.top();
 				TAC_add_counter.pop();
                                string v12 = "t" + v2;
			        string v3 = TAC_Addop_counter.top();
				TAC_Addop_counter.pop();

                                if(TAC_add_counter.empty()) {
                			finalV1 = v12;
		                	finalVop = v3;
                			finalV2 = v11;
		                 	topLarger = tt;
				        break;
				 } 
                               else { 
                                        ofile1 << "t" << top <<  " : = " << v12 << " " << v3 << " " << v11 << endl; 
	 				ostringstream buffer1;
					buffer1 << top;
		         		TAC_add_counter.push(buffer1.str()); top++; 
           			 }  
                                }  
	                     }
           | SimpleExpressionFirstPart SimpleExpression { ofile << "Expression" << endl; variableAssignedTo = "boolean";
			finalV2 = "t" + TAC_add_counter.top(); TAC_add_counter.pop();
			finalVop = TAC_Addop_counter.top(); TAC_Addop_counter.pop();
			finalV1 = "t" + TAC_add_counter.top(); TAC_add_counter.pop();	
		 }
	   ;




SimpleExpressionFirstPart : SimpleExpression1 RelationalOp{
				TAC_Addop_counter.push(relOp); 
			  }
			 ;


SimpleExpression1 : SimpleExpression {
	   	}
		  ;

SimpleExpression : Sign Term {             
				processFactor();
				ostringstream buffer1; buffer1 << TAC_temp_counter; 
				TAC_add_counter.push(buffer1.str());	
                                                       }  
		 | SimpleExpression AddOp Term { processFactor(); 
                                ostringstream buffer1; buffer1 << TAC_temp_counter;
                                TAC_add_counter.push(buffer1.str());   
			}
                 ;


RelationalOp : EQUAL { ofile << "RelationalOp" << endl; relOp = "=";}
             | LT  { ofile << "RelationalOp" << endl; relOp = "<"; }
             | LE  { ofile << "RelationalOp" << endl; relOp = "<="; }
             | GT  { ofile << "RelationalOp" << endl; relOp = ">"; }
             | GE  { ofile << "RelationalOp" << endl; relOp = ">="; } 
             | NOTEQUAL	{ ofile << "RelationalOp" << endl; relOp = "<>"; } 
             ;

AddOp : PLUS { ofile << "AddOp" << endl; TAC_Addop_counter.push("+"); } 
      | MINUS { ofile << "AddOp" << endl; TAC_Addop_counter.push("-"); } 
      | OR { ofile << "AddOp" << endl; TAC_Addop_counter.push("or"); } 
      ;	


Term : Factor {  					
              } 
 
     | Term MultOp Factor {  
              singleFactor = false;
        }                          
     ;


MultOp : MULTIPLICATION { ofile << "MultOp" << endl; TAC_operator = "*";  }
       | DIV { ofile << "MultOp" << endl; TAC_operator = "/";  } 
       | MOD { ofile << "MultOp" << endl; TAC_operator = "%";  }
       | AND { ofile << "MultOp" << endl; TAC_operator = "and";  } 
       ;


Factor : INT { ofile << "Factor" << endl; variableAssignedTo = "integer"; 
			                                                 
         if( (TAC_variable1 =="") && (TAC_flag == 0) )
            TAC_variable1 = $1; else TAC_variable2 = $1;
	     } 

       | STRING { ofile << "Factor" << endl; variableAssignedTo = "string"; }
       | Variable { ofile << "Factor" << endl; TAC_flag++;   }
       | FunctionReference { ofile << "Factor" << endl;  } 						
       | NOT Factor { ofile << "Factor" << endl; TAC_notflag++; } 
       | LPAREN Expression RPAREN { ofile << "Factor" << endl; }
       ;

FunctionReference : ID LPAREN ActualParameterList RPAREN { 
		ofile << "FunctionReference" << endl; 
		variableAssignedTo = $1;
	        variableToCheck = $1; 
	        checkVariable();       		
                ofile1 << "param ";
                for(int i=0; i<parameters.size(); i++)
		ofile1 << parameters.at(i) << " ";
      		ofile1 << endl; 
       		TAC_variable1 = "funcall ";  TAC_variable1.append($1);    parameters.clear();                                    
		 } ;


Variable : ID ComponentSelection { ofile << "Variable" << endl; variableToCheck = $1; checkVariable(); if(variableBeingAssigned=="")
           variableBeingAssigned=$1; else variableAssignedTo=$1;
	   if( (TAC_variable1 =="") && (TAC_flag == 0) ) { 
	     TAC_variable1 = $1;
	     TAC_variable1.append(variableDot); variableDot="";
           } else {  
	     TAC_variable2 = $1; 
	     TAC_variable2.append(variableDot); variableDot="";
 	   }								
         }								
         ;


ComponentSelection  : DOT ID ComponentSelection { ofile << "ComponentSelection" << endl; 
				string temp = ".";
				temp.append($2);
				temp.append(variableDot);	
				variableDot = temp;	
				}
				 
		    | LeftExpression Expression RBRAC ComponentSelection { ofile << "ComponentSelection" << endl;            
				if(tempString!="") {
				TAC_variable1 = tempString;
				TAC_operator = tempOperator;
				TAC_flag = tempFlag;
				tempString="";
				}
				
				string temp = "[t";
			
				stringstream ss1; ss1 << TAC_temp_counter;
                                temp.append(ss1.str()); temp.append("]");
				temp.append(variableDot); variableDot = temp;
				comingFrom = true;
		       }  
		    | { ofile << "ComponentSelection" << endl; }
		    ;



LeftExpression : LBRAC {        
				if(TAC_variable1!="") {
					tempString = TAC_variable1; 
					tempOperator = TAC_operator;
					TAC_variable1="";
					tempFlag = TAC_flag;
					TAC_flag = 0;
				} 
				comingFrom = false;
		 }
	       ;
	
ActualParameterList : ActualParameterListExist { ofile << "ActualParameterList" << endl; }
		    |
		    ;
	
ActualParameterListExist : Expression  {
       topLarger++;  	 
       stringstream ss;       
       ss << topLarger; 	
       	
       ofile1 << "t" << (topLarger) <<  " : = " <<  finalV1 << " " << finalVop << " " << finalV2 << endl;
       finalV1 = "";
       finalVop = "";
       finalV2 = "";
       string temp = "t";
 
       temp.append(ss.str());
       parameters.push_back(temp);	
       TAC_temp_counter = topLarger;
			  } 
		         | ActualParameterListExist COMMA Expression 
{
       topLarger++;
       stringstream ss;        
       ss << topLarger;
       string check = "t" + ss.str();
       if((check).compare(finalV1)!=0) {
       ofile1 << "t" << (topLarger) <<  " : = " <<  finalV1 << " " << finalVop << " " << finalV2 << endl;
       } 	
       finalV1 = "";
       finalVop = "";
       finalV2 = "";
       string temp = "t";
       temp.append(ss.str());
       parameters.push_back(temp);	
       TAC_temp_counter = topLarger;
}
		         ;

IdentifierList  : ID { ofile << "IdentifierList" << endl;  arg3.push_back($1);  arg6.push_back($1); variableList.push_back($1); identifierList.push_back($1); }
		| IdentifierList COMMA ID { arg3.push_back($3);  arg6.push_back($3); variableList.push_back($3);  identifierList.push_back($3); }
		; 

%%


int main(int argc, char **argv)
{
  if ((argc > 1) && (freopen(argv[1], "r", stdin) == NULL))
  {
    cerr << argv[0] << ": File " << argv[1] << " cannot be opened.\n";
    return 0;
  }

  ofile.open("rules.out");  
  ofile1.open("a.txt");
  yyparse();
  ofile1.close();
  ofile.close();


  ofstream symbolFile;
  symbolFile.open("symtable.out");
  symbolFile << "------------SYMBOL TABLE ------------" << endl;
  symbolFile << "Identifier " << "            " << "Type" << endl;
  map<string,string>::iterator it0;
  for(it0 = globalSymbolTable.begin(); it0 != globalSymbolTable.end(); it0++) {
    symbolFile << (*it0).first  << "                   " << (*it0).second << endl;
  }


  symbolFile << "------------SYMBOL TABLE ------------" << endl;
  symbolFile << "Identifier " << "            " << "Type" << endl;
  map<string,string>::iterator itt;
  for(itt = symbolTableFunctions.begin(); itt != symbolTableFunctions.end(); itt++) {
    symbolFile << (*itt).first  << "                   " << (*itt).second << endl;
  }


  symbolFile << "------------SYMBOL TABLE ------------" << endl;
  symbolFile << "Identifier " << "            " << "Type" << endl;
  map<string,string>::iterator it;
  for(it = symbolTableBasicTypes.begin(); it != symbolTableBasicTypes.end(); it++) {
    symbolFile << (*it).first  << "                   " << (*it).second << endl;
  }

  symbolFile << "------------SYMBOL TABLE ------------" << endl;
  symbolFile << "Identifier " << "            " << "Type" << endl;
  map<string,RecordsFields>::iterator it1;
  unordered_map<string,string>::iterator it2;
  for(it1 = symbolTableRecordTypes.begin(); it1 != symbolTableRecordTypes.end(); it1++) {
    symbolFile << (*it1).first  << "                   " << endl;
    symbolFile << "Fields are:" << endl;
    for(it2 = (((*it1).second).begin()); it2!= (((*it1).second).end()); it2++) {
      symbolFile << (*it2).first << " " << (*it2).second << endl;
    }
  }


  symbolFile << "------------SYMBOL TABLE ------------" << endl;
  symbolFile << "Identifier " << "            " << "Type" << endl;
  map<string,ArrayMiscellaneos>::iterator it3;
  for(it3 = symbolTableArrayTypes.begin(); it3 != symbolTableArrayTypes.end(); it3++) {
    symbolFile << (*it3).first  << "Lower:" << ((*it3).second).l << "Higher:" << ((*it3).second).u << "Type:" << ((*it3).second).type << endl;
  }


  symbolFile << "\n-----------SYMBOL TABLE FOR CONSTANTS-----------" << std::endl;
  symbolFile << "TOKEN " << "         " << "LEXEME"  << std::endl;
  for(it = ConstantsSymTab.begin(); it != ConstantsSymTab.end(); it++) {
    symbolFile << (*it).second << "          " << (*it).first << endl;
  }

  symbolFile.close();
  return 0;
}

void yyerror(const char *s) {
	cout << "Parser Error!  Message: " << s << endl;
}

void insertType() {
symbolTableBasicTypes.insert(pair<string,string> (arg1, arg2));
arg1="";
arg2="";
arg3.clear();
arg4="";
variableList.clear();
variableType = "";
identifierList.clear();
typeofIdentifier = "";
}


void insertGlobalSymbolTable() {
globalSymbolTable.insert(pair<string,string> (global1,global2));
global1="";
global2="";
}


void insertIntoFunctionSymbolTable() {
symbolTableFunctions.insert(pair<string,string> (global1,global2));
global1="";
global2="";
}


void insertRecordType() {
symbolTableRecordTypes.insert(pair<string,RecordsFields> (arg1,recordDetails));
recordDetails.clear();
}

void setLower() {
lower = toReg;
toReg = "";
}


void setHigher() {
  higher = toReg;
  toReg = "";
}

void insertArrayType() {
ArrayMiscellaneos temp;
temp.l = lower;
temp.u = higher;
temp.type = typeOfArray;

symbolTableArrayTypes.insert(pair<string,ArrayMiscellaneos> (arg1,temp));

typeOfArray = "";
lower = "";
higher = "";
}




void insertVar() {
for(int i=0;i<(int)arg3.size();i++)
recordDetails.insert(pair<string,string> (arg3.at(i), arg4));
arg1="";arg2="";arg3.clear();arg4="";
}


void insertVariable() {

map<string,string>::iterator it;
for(int i=0;i<(int)variableList.size();i++) {
  it = globalSymbolTable.find(variableList.at(i));
  if(it!=globalSymbolTable.end()) {
    cout << "Error : Multiple Declarations: Variable " << variableList.at(i) << " has been declared before of type : " << (*it).second << endl;
  }
  else 
    globalSymbolTable.insert(pair<string,string> (variableList.at(i),variableType));
}



variableList.clear();
variableType = "";

identifierList.clear();
typeofIdentifier = "";

}


void insertFunc() {

map<string,string>::iterator it;
for(int i=0;i<(int)identifierList.size();i++) {
   
  it = temporarySymbolTable.find(identifierList.at(i));
  if(it!=temporarySymbolTable.end()) 
        cout << "Error : Multiple Declarations: Variable " << identifierList.at(i) << " has been declared before of type : " << (*it).second << endl;
  else {
    temporarySymbolTable.insert(pair<string,string> (identifierList.at(i),variableType));
   }

}

arg1=""; arg2=""; arg3.clear(); arg4="";arg5="";arg7="";arg8.clear();arg6.clear();
identifierList.clear();
typeofIdentifier = "";
}


void checkVariable() {


map<string,string>::iterator it;
it = temporarySymbolTable.find(variableToCheck);
if(it==temporarySymbolTable.end()) {
  it = globalSymbolTable.find(variableToCheck);
  if(it == globalSymbolTable.end()) {
    it = symbolTableFunctions.find(variableToCheck);
    if(it == symbolTableFunctions.end()) {
      ifundeclared = true;      
      cout << "Error : Variable " << variableToCheck << " is used but not declared " << endl;
    }
  }
}
  
variableToCheck = "";
}



void processFactor() {
  if(singleFactor == true) {
    		  TAC_temp_counter++;
                  if(TAC_minusflag == 1) {
                    ofile1 << "t" << TAC_temp_counter << " : = -" << TAC_variable1 << endl;  TAC_minusflag = 0; } 
                  else if (TAC_notflag == 1) {
		    ofile1 << "t" << TAC_temp_counter << " : = not " << TAC_variable1 << endl;  TAC_notflag = 0; 
		  } 
		  else  
                    ofile1 << "t" << TAC_temp_counter << " : = " << TAC_variable1 << endl;  
                  TAC_variable1 = ""; TAC_variable2 = ""; TAC_operator = " "; TAC_flag = 0;

  } else {
	 TAC_temp_counter++;
         if (TAC_minusflag == 1) { ofile1 << "t" << TAC_temp_counter << " : = -" << TAC_variable2 << endl;
           TAC_temp_counter++;
	   if(TAC_notflag == 1) {  ofile1 << "t" << TAC_temp_counter << " : = not " << TAC_variable1 << endl;  TAC_notflag = 0; 
	 TAC_temp_counter++;
	 ofile1 << "t" <<  TAC_temp_counter << " : = " << "t" << (TAC_temp_counter-2) << " " << TAC_operator << " t" << (TAC_temp_counter-1) << endl;
	 TAC_minusflag = 0;
         TAC_variable1 = ""; TAC_variable2 = ""; TAC_operator = " "; TAC_flag = 0;

	   }				
	   else {	
	
           ofile1 << "t" <<  TAC_temp_counter << " : = " << "t" << (TAC_temp_counter-1) << "  " << TAC_operator << "  " << TAC_variable1 << endl;
           TAC_minusflag = 0;
           TAC_variable1 = ""; TAC_variable2 = ""; TAC_operator = " "; TAC_flag = 0;
             } 
           } 

	  else if (TAC_notflag >= 1) {
 
          if(TAC_notflag == 1) {
	    ofile1 << "t" << TAC_temp_counter << " : = not " << TAC_variable2 << endl;
            TAC_temp_counter++;
            ofile1 << "t" <<  TAC_temp_counter << " : = " << "t" << (TAC_temp_counter-1) << "  " << TAC_operator << "  " << TAC_variable1 << endl;
            TAC_minusflag = 0; TAC_notflag=0;
            TAC_variable1 = ""; TAC_variable2 = ""; TAC_operator = " "; TAC_flag = 0;
	   }   
	  if(TAC_notflag == 2) {
	    ofile1 << "t" << TAC_temp_counter << " : = not " << TAC_variable2 << endl;
            TAC_temp_counter++;
	    ofile1 << "t" << TAC_temp_counter << " : = not " << TAC_variable1 << endl;
	    TAC_temp_counter++;
        ofile1 << "t" <<  TAC_temp_counter << " : = " << "t" << (TAC_temp_counter-2) << "  " << TAC_operator << "  " << (TAC_temp_counter-1) << endl;
            TAC_minusflag = 0; TAC_notflag=0;
            TAC_variable1 = ""; TAC_variable2 = ""; TAC_operator = " "; TAC_flag = 0;
           }	


	  } 
	  else {
                ofile1 << "t" << TAC_temp_counter <<  " : = " <<  TAC_variable1 << "  " << TAC_operator << "  " << TAC_variable2 << endl;
                TAC_variable1 = ""; TAC_variable2 = ""; TAC_operator = " "; TAC_flag = 0;
             }

              singleFactor = false;


  }	





}

void checkTypeEquivalance() {
bool flag = true;
string temp = "";
map<string,string>::iterator itFunctions;

itFunctions = symbolTableFunctions.find(variableBeingAssigned);


if(itFunctions != symbolTableFunctions.end()) {
   //Get the Type of variableBeingAssigned
  string temp = (*itFunctions).second;
  
  if(!( (temp.compare("integer")==0) || (temp.compare("string")==0) || (temp.compare("boolean")==0) ) )  {
                  
                  map<string,string>::iterator basicIt1;
                  map<string,struct ArrayMiscellaneos>::iterator arrayIt1;
                  map<string, RecordsFields>::iterator recordIt1;
                  basicIt1 = symbolTableBasicTypes.find(temp);
                  arrayIt1 = symbolTableArrayTypes.find(temp);
                  recordIt1 = symbolTableRecordTypes.find(temp);
                  if(basicIt1 != symbolTableBasicTypes.end()) {
                        temp = (*basicIt1).second;
                  } else if (arrayIt1 != symbolTableArrayTypes.end()) {

                  }  else {


                  itFunctions = globalSymbolTable.find(temp);
                  if(itFunctions!=globalSymbolTable.end()) {
                    temp = (*itFunctions).second;
                                                

                  if(!( (temp.compare("integer")==0) || (temp.compare("string")==0) || (temp.compare("boolean")==0) ) )  {
                  map<string,string>::iterator basicIt1;
                  map<string,struct ArrayMiscellaneos>::iterator arrayIt1;
                  map<string, RecordsFields>::iterator recordIt1;
                  basicIt1 = symbolTableBasicTypes.find(temp);
                  arrayIt1 = symbolTableArrayTypes.find(temp);
                  recordIt1 = symbolTableRecordTypes.find(temp);
                  if(basicIt1 != symbolTableBasicTypes.end()) {
                        temp = (*basicIt1).second;
                  } else if (arrayIt1 != symbolTableArrayTypes.end()) {

                  } else {

                  }
           }
        } else {  flag = false; }
  }
  }
 
  //Get the Type of variableAssignedTo
    map<string,string>::iterator local;
    local = temporarySymbolTable.find(variableAssignedTo);
    string temp1="";
    if(local !=temporarySymbolTable.end()) {
      temp1 = (*local).second;

      if(!( (temp1.compare("integer")==0) || (temp.compare("boolean")==0) || (temp1.compare("string")==0))) {

                  map<string,string>::iterator basicIt1;
                  map<string,struct ArrayMiscellaneos>::iterator arrayIt1;
                  map<string, RecordsFields>::iterator recordIt1;
                  basicIt1 = symbolTableBasicTypes.find(temp1);
                  arrayIt1 = symbolTableArrayTypes.find(temp1);
                  recordIt1 = symbolTableRecordTypes.find(temp1);
                  if(basicIt1 != symbolTableBasicTypes.end()) {
                        temp1 = (*basicIt1).second;
                  } else if (arrayIt1 != symbolTableArrayTypes.end()) {

                  } else {

                  }



       }
    } //Get the type from global table
        else {
       local = globalSymbolTable.find(variableAssignedTo);
       if(local != globalSymbolTable.end()) {
        temp1 = (*local).second;
      if(!( (temp1.compare("integer")==0) || (temp.compare("boolean")==0) || (temp1.compare("string")==0))) {

                  map<string,string>::iterator basicIt1;
                  map<string,struct ArrayMiscellaneos>::iterator arrayIt1;
                  map<string, RecordsFields>::iterator recordIt1;
                  basicIt1 = symbolTableBasicTypes.find(temp1);
                  arrayIt1 = symbolTableArrayTypes.find(temp1);
                  recordIt1 = symbolTableRecordTypes.find(temp1);
                  if(basicIt1 != symbolTableBasicTypes.end()) {
                        temp1 = (*basicIt1).second;
                  } else if (arrayIt1 != symbolTableArrayTypes.end()) {

                  } else {

                  }

        }


    } else { flag = false;}
   }
                     
                        if(flag == true) {
                         if(temp.compare(temp1)!=0)
                          cout << "Error : Type MisMatch:" << " Function : " << variableBeingAssigned << " return type is "
                          << temp << " but returns " << variableAssignedTo << " which has a return type of " << temp1 << endl; } 

 


} //First If

else {

itFunctions = symbolTableFunctions.find(variableAssignedTo);
if(itFunctions !=symbolTableFunctions.end()) {
  /// Get The Type of variableAssignedTo
  temp = (*itFunctions).second;   
  if(!( (temp.compare("integer")==0) || (temp.compare("string")==0) || (temp.compare("boolean")==0) ) )  {

                  itFunctions = globalSymbolTable.find(temp);
                  if(itFunctions!=globalSymbolTable.end()) {
                    temp = (*itFunctions).second; 
                 
                  if(!( (temp.compare("integer")==0) || (temp.compare("string")==0) || (temp.compare("boolean")==0) ) )  {
                  map<string,string>::iterator basicIt1;
                  map<string,struct ArrayMiscellaneos>::iterator arrayIt1;
                  map<string, RecordsFields>::iterator recordIt1;
                  basicIt1 = symbolTableBasicTypes.find(temp);
                  arrayIt1 = symbolTableArrayTypes.find(temp);
                  recordIt1 = symbolTableRecordTypes.find(temp);
                  if(basicIt1 != symbolTableBasicTypes.end()) {
                        temp = (*basicIt1).second;
                  } else if (arrayIt1 != symbolTableArrayTypes.end()) {

                  } else {

                  }
           }
        } else {  flag = false; }
  }  

  // Next Find the Type of variableBeingAssigned
    map<string,string>::iterator local;
    local = temporarySymbolTable.find(variableBeingAssigned);
    string temp1="";
    if(local !=temporarySymbolTable.end()) {
      temp1 = (*local).second;

      if(!( (temp1.compare("integer")==0) || (temp.compare("boolean")==0) || (temp1.compare("string")==0))) {
                
                  map<string,string>::iterator basicIt1;
                  map<string,struct ArrayMiscellaneos>::iterator arrayIt1;
                  map<string, RecordsFields>::iterator recordIt1;
                  basicIt1 = symbolTableBasicTypes.find(temp1);
                  arrayIt1 = symbolTableArrayTypes.find(temp1);
                  recordIt1 = symbolTableRecordTypes.find(temp1);
                  if(basicIt1 != symbolTableBasicTypes.end()) {
                        temp1 = (*basicIt1).second;
                  } else if (arrayIt1 != symbolTableArrayTypes.end()) {

                  } else {

                  }



       }

    } //Get the type from global table
    else {
       local = globalSymbolTable.find(variableBeingAssigned);
       if(local != globalSymbolTable.end()) {   
        temp1 = (*local).second;

      if(!( (temp1.compare("integer")==0) || (temp.compare("boolean")==0) || (temp1.compare("string")==0))) {

                  map<string,string>::iterator basicIt1;
                  map<string,struct ArrayMiscellaneos>::iterator arrayIt1;
                  map<string, RecordsFields>::iterator recordIt1;
                  basicIt1 = symbolTableBasicTypes.find(temp1);
                  arrayIt1 = symbolTableArrayTypes.find(temp1);
                  recordIt1 = symbolTableRecordTypes.find(temp1);
                  if(basicIt1 != symbolTableBasicTypes.end()) {
                        temp1 = (*basicIt1).second;
                  } else if (arrayIt1 != symbolTableArrayTypes.end()) {

                  } else {

                  }

        }


    } else { flag = false;}  
   } 
                        if(flag == true) {
                         if(temp.compare(temp1)!=0)
                          cout << "Error : Type MisMatch:" << " Variable : " << variableBeingAssigned << " is declared of type "
                          << temp1 << " but assigned to a function " << variableAssignedTo << " which has a return type of " << temp << endl; }




} else { 

  map<string,string>::iterator local;
  map<string,string>::iterator basicIt;
  map<string,struct ArrayMiscellaneos>::iterator arrayIt;
  map<string, RecordsFields>::iterator recordIt;
      
  local = temporarySymbolTable.find(variableBeingAssigned);
  if(local != temporarySymbolTable.end()) {  //Variable has been declared in local scope
  string temp = (*local).second;
  if( (temp.compare("integer")==0) || (temp.compare("boolean")==0) || (temp.compare("string")==0)) {

       if( (variableAssignedTo.compare("integer")==0) || (variableAssignedTo.compare("boolean")==0) || (variableAssignedTo.compare("string")==0) ) {

             if(temp.compare(variableAssignedTo) != 0)
             cout << "Error : Type MisMatch:" << " Variable : " << variableBeingAssigned << " is declared of type " << temp << 
               " but assigned to " << variableAssignedTo << endl;

        } else {  //Find right parts type
     
    //******************************************************************
         basicIt = temporarySymbolTable.find(variableAssignedTo);  //Check the type of right part

        if(basicIt != temporarySymbolTable.end()) { //If Variable declared in local scope
           string type1 = (*basicIt).second;
           if( (type1.compare("integer")==0) || (type1.compare("boolean")==0) || (type1.compare("string")==0)) {
                      if(type1.compare(temp)!=0)
                        cout << "Error: Type MisMatch: " << " Variable : " << variableBeingAssigned << " is declared of type "
                        << temp << " but assigned to " << variableAssignedTo << " which has a type of" << type1 << endl;

           } else {  //Get its Type from Outside
                  map<string,string>::iterator basicIt1;
                  map<string,struct ArrayMiscellaneos>::iterator arrayIt1;
                  map<string, RecordsFields>::iterator recordIt1;
                  basicIt1 = symbolTableBasicTypes.find(type1);
                  arrayIt1 = symbolTableArrayTypes.find(type1);
                  recordIt1 = symbolTableRecordTypes.find(type1);
                  if(basicIt1 != symbolTableBasicTypes.end()) {
                   string type2 = (*basicIt1).second;
                   if(type2.compare(temp)!=0) {
                        cout << "Error : Type MisMatch:" << " Variable : " << variableBeingAssigned << " is declared of type "
                        << temp << " but assigned to " << variableAssignedTo << " which has a type of " << type2 << endl;
                   }
                  } else if (arrayIt1 != symbolTableArrayTypes.end()) {}
                    else {}


           }
         }  else {  //Check the Global Table for right part
              basicIt = globalSymbolTable.find(variableAssignedTo);
              if(basicIt != globalSymbolTable.end()) { //
              string type1 = (*basicIt).second;
              if( (type1.compare("integer")==0) || (type1.compare("boolean")==0) || (type1.compare("string")==0)) {
                if(type1.compare(temp)!=0)
                        cout << "Error: Type MisMatch: " << " Variable : " << variableBeingAssigned << " is declared of type "
                        << temp << " but assigned to " << variableAssignedTo << " which has a type of " << type1 << endl;
              } else {
                  map<string,string>::iterator basicIt1;
                  map<string,struct ArrayMiscellaneos>::iterator arrayIt1;
                  map<string, RecordsFields>::iterator recordIt1;
                  basicIt1 = symbolTableBasicTypes.find(type1);
                  arrayIt1 = symbolTableArrayTypes.find(type1);
                  recordIt1 = symbolTableRecordTypes.find(type1);
                  if(basicIt1 != symbolTableBasicTypes.end()) {
                   string type2 = (*basicIt1).second;
                   if(type2.compare(temp)!=0) {
                        cout << "Error: Type MisMatch: " << " Variable : " << variableBeingAssigned << " is declared of type "
                        << temp << " but assigned to " << variableAssignedTo << " which has a type of " << type2 << endl;
                   }
                  } else if (arrayIt1 != symbolTableArrayTypes.end()) {}
                    else {}


              }
           }  //

         }
       


    // **************************************************************

        }


  }  //Type in Declaration is indirect  of left part
  else {
    basicIt = symbolTableBasicTypes.find(temp);
    arrayIt = symbolTableArrayTypes.find(temp);
    recordIt = symbolTableRecordTypes.find(temp);
    if(basicIt != symbolTableBasicTypes.end()) {  //If Argument indirect in basic Type
        string type = (*basicIt).second;
       
       if( (variableAssignedTo.compare("integer")==0) || (variableAssignedTo.compare("boolean")==0) || (variableAssignedTo.compare("string")==0) ) {
             if(type.compare(variableAssignedTo) != 0)
             cout << "Error : Type MisMatch:" << " Variable : " << variableBeingAssigned << " is declared of type " << type <<
               " but assigned to " << variableAssignedTo << endl;
       } else {  


        basicIt = temporarySymbolTable.find(variableAssignedTo);  //Check the type of right part
           
        if(basicIt != temporarySymbolTable.end()) { //If Variable declared in local scope
           string type1 = (*basicIt).second;
           if( (type1.compare("integer")==0) || (type1.compare("boolean")==0) || (type1.compare("string")==0)) {
                      if(type1.compare(type)!=0)
                        cout << "Error : Type MisMatch:" << " Variable : " << variableBeingAssigned << " is declared of type "
                        << type << " but assigned to " << variableAssignedTo << endl;

           } else {  //Get its Type from others
                  map<string,string>::iterator basicIt1; 
                  map<string,struct ArrayMiscellaneos>::iterator arrayIt1;
                  map<string, RecordsFields>::iterator recordIt1;
                  basicIt1 = symbolTableBasicTypes.find(type1);
                  arrayIt1 = symbolTableArrayTypes.find(type1);
                  recordIt1 = symbolTableRecordTypes.find(type1);
                  if(basicIt1 != symbolTableBasicTypes.end()) {
                   string type2 = (*basicIt1).second;
                   if(type2.compare(type)!=0) {
                        cout << "Error : Type MisMatch:" << " Variable : " << variableBeingAssigned << " is declared of type "
                        << type << " but assigned to " << variableAssignedTo << " which has a type of " << type2 << endl;
                   } 
                  } 
                    
                 else if (arrayIt1 != symbolTableArrayTypes.end()) {}
                 else {}


           }   
         }  else {  //Check the Global Table for right part

              //********************************************
                            basicIt = globalSymbolTable.find(variableAssignedTo);
             if(basicIt != globalSymbolTable.end()) { //
              string type1 = (*basicIt).second;
              if( (type1.compare("integer")==0) || (type1.compare("boolean")==0) || (type1.compare("string")==0)) {
                if(type1.compare(type)!=0)
                        cout << "Error: Type MisMatch:" << " Variable : " << variableBeingAssigned << " is declared of type "
                        << type << " but assigned to " << variableAssignedTo << " which has a type of " << type1 << endl;
              } else {
                  map<string,string>::iterator basicIt1;
                  map<string,struct ArrayMiscellaneos>::iterator arrayIt1;
                  map<string, RecordsFields>::iterator recordIt1;
                  basicIt1 = symbolTableBasicTypes.find(type1);
                  arrayIt1 = symbolTableArrayTypes.find(type1);
                  recordIt1 = symbolTableRecordTypes.find(type1);
                  if(basicIt1 != symbolTableBasicTypes.end()) {
                   string type2 = (*basicIt1).second;
                   if(type2.compare(type)!=0) {
                        cout << "Error: Type MisMatch:" << " Variable : " << variableBeingAssigned << " is declared of type "
                        << type << " but assigned to " << variableAssignedTo << " which has a type of " << type2 << endl;
                   }
                  } else if (arrayIt1 != symbolTableArrayTypes.end()) {}
                    else {}


              }
           
            } else {return;} //
              //*******************************************
         }
       }
     
    } 
    else if (arrayIt != symbolTableArrayTypes.end()) { //If ArgumentType Indirect is of Array
      
    }
    else {  //If ArgumentType indirect is of Type record

    }
 
  }  //Indirect Declaration Closes 
 } else {  //Check the Global Table if left part is defined outside
  
      local = globalSymbolTable.find(variableBeingAssigned);

                      if(basicIt != globalSymbolTable.end()) { //
   
      string type1 = (*local).second;
      if( (type1.compare("integer")==0) || (type1.compare("boolean")==0) || (type1.compare("string")==0))  {
                              //****************
        if( (variableAssignedTo.compare("integer")==0) || (variableAssignedTo.compare("boolean")==0) || (variableAssignedTo.compare("string")==0) ) {
             if(type1.compare(variableAssignedTo) != 0)
             cout << "Error: Type MisMatch:" << " Variable : " << variableBeingAssigned << " is declared of type " << type1 <<
               " but assigned to " << variableAssignedTo << endl;
         } else {
                 ///////////////////////////
            basicIt = temporarySymbolTable.find(variableAssignedTo);  //Check the type of right part
            
            if(basicIt != temporarySymbolTable.end()) { //If Variable declared in local scope
            string type2 = (*basicIt).second;
           if( (type2.compare("integer")==0) || (type2.compare("boolean")==0) || (type2.compare("string")==0)) {
                      if(type2.compare(type1)!=0)
                        cout << "Error: Type MisMatch:" << " Variable : " << variableBeingAssigned << " is declared of type "
                        << type1 << " but assigned to " << variableAssignedTo << " which has a type of " << type2 << endl;

           } else {  //Get its Type from Outside
                  map<string,string>::iterator basicIt1;
                  map<string,struct ArrayMiscellaneos>::iterator arrayIt1;
                  map<string, RecordsFields>::iterator recordIt1;
                  basicIt1 = symbolTableBasicTypes.find(type1);
                  arrayIt1 = symbolTableArrayTypes.find(type1);
                  recordIt1 = symbolTableRecordTypes.find(type1);
                  if(basicIt1 != symbolTableBasicTypes.end()) {
                   string type3 = (*basicIt1).second;
                   if(type3.compare(type1)!=0) {
                        cout << "Error : Type MisMatch:" << " Variable : " << variableBeingAssigned << " is declared of type "
                        << type1 << " but assigned to " << variableAssignedTo << " which has a type of " << type3 << endl;
                   }
                  } else if (arrayIt1 != symbolTableArrayTypes.end()) {}
                    else {}


           }
                    }  else {  //Check the Global Table for right part

              
              basicIt = globalSymbolTable.find(variableAssignedTo);
              if(basicIt != globalSymbolTable.end()) { //
              string type2 = (*basicIt).second;
              if( (type2.compare("integer")==0) || (type2.compare("boolean")==0) || (type2.compare("string")==0)) {
                if(type2.compare(type1)!=0)
                        cout << "Error: Type MisMatch:" << " Variable : " << variableBeingAssigned << " is declared of type "
                        << type1 << " but assigned to " << variableAssignedTo << " which has a type of " << type2 << endl;
              } else {
                  cout << "Enter here:" << endl;
                  map<string,string>::iterator basicIt1;
                  map<string,struct ArrayMiscellaneos>::iterator arrayIt1;
                  map<string, RecordsFields>::iterator recordIt1;
                  basicIt1 = symbolTableBasicTypes.find(type1);
                  arrayIt1 = symbolTableArrayTypes.find(type1);
                  recordIt1 = symbolTableRecordTypes.find(type1);
                  if(basicIt1 != symbolTableBasicTypes.end()) {
                   string type3 = (*basicIt1).second;
                   if(type3.compare(type1)!=0) {
                        cout << "Type MisMatch:" << " Variable :" << variableBeingAssigned << " is declared of type "
                        << type1 << " but assigned to " << variableAssignedTo << " which has a type of " << type3 << endl;
                   }
                  } else if (arrayIt1 != symbolTableArrayTypes.end()) {}
                    else {}


              }




           } else {return;} //

         } //.

               













              ///////////////////////
           
               



         }

                              //*************88
              } else {
                  map<string,string>::iterator basicIt1;
                  map<string,struct ArrayMiscellaneos>::iterator arrayIt1;
                  map<string, RecordsFields>::iterator recordIt1;
                  basicIt1 = symbolTableBasicTypes.find(type1);
                  arrayIt1 = symbolTableArrayTypes.find(type1);
                  recordIt1 = symbolTableRecordTypes.find(type1);
                  if(basicIt1 != symbolTableBasicTypes.end()) {
                   string type1 = (*basicIt1).second;



 



         if( (variableAssignedTo.compare("integer")==0) || (variableAssignedTo.compare("boolean")==0) || (variableAssignedTo.compare("string")==0) ) {
             if(type1.compare(variableAssignedTo) != 0)
             cout << "Error: Type MisMatch:" << " Variable : " << variableBeingAssigned << " is declared of type " << type1 <<
               " but assigned to " << variableAssignedTo << endl;
         } else {
                 ///////////////////////////
                             basicIt = temporarySymbolTable.find(variableAssignedTo);  //Check the type of right part
            
            if(basicIt != temporarySymbolTable.end()) { //If Variable declared in local scope
            string type2 = (*basicIt).second;
           if( (type2.compare("integer")==0) || (type2.compare("boolean")==0) || (type2.compare("string")==0)) {
                      if(type2.compare(type1)!=0)
                        cout << "Error: Type MisMatch:" << " Variable :" << variableBeingAssigned << " is declared of type "
                        << type1 << " but assigned to " << variableAssignedTo << " which has a type of " << type2 << endl;

           } else {  //Get its Type from Outside
                  map<string,string>::iterator basicIt1;
                  map<string,struct ArrayMiscellaneos>::iterator arrayIt1;
                  map<string, RecordsFields>::iterator recordIt1;
                  basicIt1 = symbolTableBasicTypes.find(type2);
                  arrayIt1 = symbolTableArrayTypes.find(type2);
                  recordIt1 = symbolTableRecordTypes.find(type2);
                  if(basicIt1 != symbolTableBasicTypes.end()) {
                   string type3 = (*basicIt1).second;
                   
                   if(type3.compare(type1)!=0) {
                        cout << "Error: Type MisMatch:" << " Variable :" << variableBeingAssigned << " is declared of type "
                        << type1 << " but assigned to " << variableAssignedTo << " which has a type of " << type3 << endl;
                   }
                  } else if (arrayIt1 != symbolTableArrayTypes.end()) {}
                    else {}
                            }
                    }  else {  //Check the Global Table for right part
              basicIt = globalSymbolTable.find(variableAssignedTo);
              if(basicIt != globalSymbolTable.end()) { //
              string type2 = (*basicIt).second;
              if( (type2.compare("integer")==0) || (type2.compare("boolean")==0) || (type2.compare("string")==0)) {
                if(type2.compare(type1)!=0)
                        cout << "Error: Type MisMatch:" << " Variable :" << variableBeingAssigned << " is declared of type "
                        << type1 << " but assigned to " << variableAssignedTo << " which has a type of " << type2 << endl;
              } else {
                  map<string,string>::iterator basicIt1;
                  map<string,struct ArrayMiscellaneos>::iterator arrayIt1;
                  map<string, RecordsFields>::iterator recordIt1;
                  basicIt1 = symbolTableBasicTypes.find(type2);
                  arrayIt1 = symbolTableArrayTypes.find(type2);
                  recordIt1 = symbolTableRecordTypes.find(type2);
                  if(basicIt1 != symbolTableBasicTypes.end()) {
                   string type3 = (*basicIt1).second;
                   if(type3.compare(type1)!=0) {
                        cout << "Error: Type MisMatch:" << " Variable :" << variableBeingAssigned << " is declared of type "
                        << type1 << " but assigned to " << variableAssignedTo << " which has a type of " << type3 << endl;
                   }
                  } else if (arrayIt1 != symbolTableArrayTypes.end()) {}
                    else {}


              }
              } //
             }

             ////////
             }

                 



















                  } else if (arrayIt1 != symbolTableArrayTypes.end()) {}
                    else {}

















              }



































   } else {return;} //


 


  }


}
}
  variableBeingAssigned = "";
  variableAssignedTo = "";

}



