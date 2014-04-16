grammar PTP;

@header {

package pt.up.fe.iart.proj1.parser;

}

map : stmt+ ;

stmt : node_stmt #node
     | edge_stmt #edge
     ;

edge_stmt : 'Edge' '(' from=node_stmt ',' to=node_stmt ',' weight=(INT|REAL) ')';

node_stmt : 'GasStation' '(' position ')'                   #GasStation
          | 'GenericLocation' '(' position ')'              #GenericLocation
          | 'PatientLocation' '(' position ')'              #PatientLocation
          | 'Filiation' '(' position ',' bool ')'           #Filiation
          ;

position : '(' x=INT ',' y=INT ')' ;
bool : 'true' | 'false' ;

INT: [0-9]+ ;
REAL: [0-9]+ '.' [0-9]* ;

WS : [ \t\r\n]+ -> skip;