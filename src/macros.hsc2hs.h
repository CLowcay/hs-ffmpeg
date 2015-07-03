
/*
  Helper macros
 */

#define hsc_test(name) \
  printf("%s = undefined\n", #name);

#define hsc_enum2(t, f, print_name, x)         \
    print_name;                               \
    printf (" :: %s\n", #t);                  \
    print_name;                               \
    printf (" = %s ", #f);                    \
    if ((x) < 0)                              \
        printf ("(%ld)\n", (long)(x));        \
    else                                      \
        printf ("%lu\n", (unsigned long)(x));

#define hsc_haskellizef(x)                                          \
    do{                                                              \
        const char *s = (x);                                       \
        int upper = 0;                                             \
        if (*s != '\0')                                            \
        {                                                          \
            putchar (tolower (*s));                                \
            ++s;                                                   \
            while (*s != '\0')                                     \
            {                                                      \
                if (*s == '_')                                     \
                    upper = 1;                                     \
                else                                               \
                {                                                  \
                    putchar (upper ? toupper (*s) : tolower (*s)); \
                    upper = 0;                                     \
                }                                                  \
                ++s;                                               \
            }                                                      \
        }                                                          \
    }while(0)

#define hsc_haskellizet(x)                                         \
    do{                                                              \
        const char *s = (x);                                       \
        int upper = 1;                                             \
        if (*s != '\0')                                            \
        {                                                          \
            while (*s != '\0')                                     \
            {                                                      \
                if (*s == '_')                                     \
                    upper = 1;                                     \
                else                                               \
                {                                                  \
                    putchar (upper ? toupper (*s) : tolower (*s)); \
                    upper = 0;                                     \
                }                                                  \
                ++s;                                               \
            }                                                      \
        }                                                          \
    }while(0)


#define hsc_begin_enum(type, val)                   \
  do                                                \
    {                                               \
      printf("data %s = \n", #type);                \
      char enum_type[255];                          \
      strcpy(enum_type, #type);                     \
      char enum_buffer[500][255];                   \
      long enum_vals[255];                          \
      int e = 0;                                    \
      printf("\t ");                                 \
      hsc_haskellizet(#val);                        \
      printf("\n");                                 \
      strcpy(enum_buffer[e], #val);                 \
      enum_vals[e++] = (long)val;
      
#define hsc_add_enum(val)                       \
  printf("\t|");                                \
  hsc_haskellizet(#val);                        \
  printf("\n");                                 \
  strcpy(enum_buffer[e], #val);                 \
  enum_vals[e++] = (long)val;

#define hsc_end_enum(deriving)                  \
  if(deriving[0] != 0){                         \
    printf(" deriving (%s)\n\n", deriving);         \
  }                                             \
  printf("instance Enum %s where\n", enum_type);    \
  int i;                                            \
  for(i = 0; i < e; ++i){                       \
    printf("\tfromEnum ");                      \
    hsc_haskellizet(enum_buffer[i]);             \
    printf(" = (%ld)\n", enum_vals[i]);             \
  }                                                 \
  for(i = 0; i < e; ++i){                       \
    printf("\ttoEnum (%ld) = ", enum_vals[i]);  \
    hsc_haskellizet(enum_buffer[i]);                                \
    printf("\n");                                                   \
  }                                                                 \
  printf("\ttoEnum id = error $ \"toEnum: fail to cast\" ++ (show id) ++ \"to %s\"\n", enum_type); \
  }while(0)
