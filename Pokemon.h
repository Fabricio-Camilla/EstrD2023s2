#include <iostream>
using namespace std;

typedef string TipoDePokemon;

struct PokeSt {
    TipoDePokemon tipo;
    int vida;
};

typedef PokeSt* Pokemon;

//Dado un tipo devuelve un pokémon con 100 % de energía.
Pokemon consPokemon(TipoDePokemon tipo);

//Devuelve el tipo de un pokémon.
TipoDePokemon tipoDePokemon(Pokemon p);

//Devuelve el porcentaje de energía.
int energia(Pokemon p);

//Le resta energía al pokémon.
void perderEnergia(int energia, Pokemon p);

//Dados dos pokémon indica si el primero, en base al tipo, es superior al segundo. Agua supera
//a fuego, fuego a planta y planta a agua. Y cualquier otro caso es falso.
bool superaA(Pokemon p1, Pokemon p2);