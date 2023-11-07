#include <iostream>
#include "Entrenador.h"


using namespace std;

Entrenador consEntrenador(string nombre, int cantidad, Pokemon* pokemon){
    Entrenador e = new EntrenadorSt;
    e -> nombre = nombre;
    e-> pokemon = pokemon;
    e -> cantPokemon = cantidad;
    return(e);
}

string nombreDeEntrenador(Entrenador e){
    return(e -> nombre);
}

int cantidadDePokemon(Entrenador e){
    return(e -> cantPokemon);
}

int cantidadDePokemonDe(TipoDePokemon tipo, Entrenador e){
    int res = 0;
    for(int i=0; i == e -> cantPokemon && tipoDePokemon(e->pokemon[i]) == tipo; i++){
        res++;
    }return res;
}

/*Pokemon pokemonNro(int i, Entrenador e);

bool leGanaATodos(Entrenador e1, Entrenador e2);*/

