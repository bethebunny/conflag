#include "conflag.h"

extern int yydebug;

int main(int argc, char **argv) {
  //yydebug = 1;
  conflag_ref file = conflag_load(argv[1]);
  printf("Loaded file!\n");
  if (argc < 3) {
    conflag_ref_print(file);
  }
  for (int i = 2; i < argc; ++i) {
    conflag_ref result = get(file.object, argv[i]);
    /*
    switch(result.type) {
      case CONFLAG_STRING:
        printf("String: '%s'\n", result.string->string); break;
      case CONFLAG_NUMBER:
        switch (result.number->type) {
          case CONFLAG_NUMBER_DOUBLE:
            printf("Double: %lf\n", result.number->d); break;
          case CONFLAG_NUMBER_INTEGER:
            printf("Integer: %ld\n", result.number->l); break;
        }; break;
      case CONFLAG_BOOLEAN:
        printf("Boolean: %s\n", result.boolean ? "true" : "false"); break;
      case CONFLAG_ERROR:
        printf("Error: %d\n", result.error); break;
      default:
        printf("Unprintable type %s\n", conflag_type_to_string(result.type));
    }
    */
    conflag_ref_print(result);
  }
}
