grammar PTP;

map : stmt+ ;

stmt : node_stmt #node
     | edge_stmt #edge
     ;

edge_stmt : 'Edge' '(' from=node_stmt ',' to=node_stmt ',' weight=(INT|REAL) ')';

node_stmt : gasStation
          | genericLocation
          | patientLocation
          | filiation
          ;

patient : 'Patient' '(' filiation? ')'
        ;

gasStation : 'GasStation' '(' position ')'
           ;

genericLocation : 'GenericLocation' '(' position ')'
                ;

patientLocation : 'PatientLocation' '(' position ',' patient ')'
                ;

filiation : 'Filiation' '(' position ',' bool ')'
          ;

position : '(' x=(INT|REAL) ',' y=(INT|REAL) ')' ;
bool : 'true' | 'false' ;

INT: [0-9]+ ;
REAL: [0-9]+ '.' [0-9]* ;

WS : [ \t\r\n]+ -> skip;
COMMENT         :   '/*' .*? '*/'       -> skip ;
LINE_COMMENT    :   '//' .*? '\r'? '\n' -> skip ;