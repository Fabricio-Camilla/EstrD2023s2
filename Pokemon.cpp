#include <iostream>
using namespace std;

#include "Pokemon.h"

Pokemon consPokemon(TipoDePokemon tipo){
    Pokemon p = new PokeSt;
    p -> tipo = tipo;
    p -> vida = 100;
    return(p);
}

TipoDePokemon tipoDePokemon(Pokemon p){
    return(p-> tipo);
}

int energia(Pokemon p){
    return(p-> vida);
}

void perderEnergia(int energia, Pokemon p){
    p -> vida -= energia;
}

bool superaA(Pokemon p1, Pokemon p2){
    if (tipoDePokemon (p1) == "Agua" && tipoDePokemon(p2) == "Fuego" ||
        tipoDePokemon (p1) == "Fuego" && tipoDePokemon(p2) == "Planta" ||
        tipoDePokemon (p1) == "Planta" && tipoDePokemon(p2) == "Agua"){
        return true;
    }return false;
}


int main(){
    Pokemon p = consPokemon("Fuego");
    perderEnergia(2,p);
    cout << energia(p);

}