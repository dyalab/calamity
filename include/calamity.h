#ifndef CALAMITY_H
#define CALAMITY_H

#define FILE_LOG(file, ... ) \
    if( file ){ \
    FILE* F = fopen(file, "a+"); \
    fprintf(F, __VA_ARGS__);            \
    fclose(F); \
    }

#endif  /* CALAMITY_H */
