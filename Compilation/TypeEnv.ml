type tEnv {

    tas :  ( int * tObject) list ;
    classes : ( string * tClasse) list;

}

and type tObject = {
    myClass = string 
}

and type tClasse {

    super : string ;

    functions :  ( string * fun_id) list ;

    
}

and fun_id = string
