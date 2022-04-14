#include <coz.h>

void cozBegin(const char* name) {
    COZ_BEGIN(name);
}

void cozEnd(const char* name) {
    COZ_END(name);
}

void cozProgressNamed(const char* name) {
    COZ_PROGRESS_NAMED(name);
}
