# test_gettext_lang() is working as expected

    Code
      test_gettext_lang()
    Output
      Test of svBase's `gettext()` and `gettextf()`:
      This should be transtlated, if 'en' language is supported.
      This is message number 3
      You asked for only one item

---

    Code
      test_gettext_lang(n = 0)
    Output
      Test of svBase's `gettext()` and `gettextf()`:
      This should be transtlated, if 'en' language is supported.
      This is message number 3
      You asked for several items

---

    Code
      test_gettext_lang(n = 1)
    Output
      Test of svBase's `gettext()` and `gettextf()`:
      This should be transtlated, if 'en' language is supported.
      This is message number 3
      You asked for only one item

---

    Code
      test_gettext_lang(n = 2)
    Output
      Test of svBase's `gettext()` and `gettextf()`:
      This should be transtlated, if 'en' language is supported.
      This is message number 3
      You asked for several items

---

    Code
      test_gettext_lang("en", n = 0)
    Output
      Test of svBase's `gettext()` and `gettextf()`:
      This should be transtlated, if 'en' language is supported.
      This is message number 3
      You asked for several items

---

    Code
      test_gettext_lang("en", n = 1)
    Output
      Test of svBase's `gettext()` and `gettextf()`:
      This should be transtlated, if 'en' language is supported.
      This is message number 3
      You asked for only one item

---

    Code
      test_gettext_lang("en", n = 2)
    Output
      Test of svBase's `gettext()` and `gettextf()`:
      This should be transtlated, if 'en' language is supported.
      This is message number 3
      You asked for several items

---

    Code
      test_gettext_lang("en_US.UTF-8", n = 2)
    Output
      Test of svBase's `gettext()` and `gettextf()`:
      This should be transtlated, if 'en_US.UTF-8' language is supported.
      This is message number 3
      You asked for several items

---

    Code
      test_gettext_lang("en", n = 9)
    Output
      Test of svBase's `gettext()` and `gettextf()`:
      This should be transtlated, if 'en' language is supported.
      This is message number 3
      You asked for several items

---

    Code
      test_gettext_lang("C", n = 0)
    Output
      Test of svBase's `gettext()` and `gettextf()`:
      This should be transtlated, if 'C' language is supported.
      This is message number 3
      You asked for several items

---

    Code
      test_gettext_lang("C", n = 1)
    Output
      Test of svBase's `gettext()` and `gettextf()`:
      This should be transtlated, if 'C' language is supported.
      This is message number 3
      You asked for only one item

---

    Code
      test_gettext_lang("C", n = 2)
    Output
      Test of svBase's `gettext()` and `gettextf()`:
      This should be transtlated, if 'C' language is supported.
      This is message number 3
      You asked for several items

---

    Code
      test_gettext_lang(NULL)
    Output
      Test of svBase's `gettext()` and `gettextf()`:
      This should be transtlated, if 'en' language is supported.
      This is message number 3
      You asked for only one item

---

    Code
      test_gettext_lang(character(0))
    Output
      Test of svBase's `gettext()` and `gettextf()`:
      This should be transtlated, if 'en' language is supported.
      This is message number 3
      You asked for only one item

---

    Code
      test_gettext_lang("zz")
    Output
      Test of svBase's `gettext()` and `gettextf()`:
      This should be transtlated, if 'zz' language is supported.
      This is message number 3
      You asked for only one item

---

    Code
      test_gettext_lang("zzzz")
    Output
      Test of svBase's `gettext()` and `gettextf()`:
      This should be transtlated, if 'zzzz' language is supported.
      This is message number 3
      You asked for only one item

---

    Code
      test_gettext_lang("fr", n = 0)
    Output
      Test des fonctions `gettext()` et `gettextf()` de svBase :
      Ceci doit être traduit si la langue 'fr' est prise en charge.
      Il s'agit du message numéro 3
      Vous n'avez demandé qu'un seul item

---

    Code
      test_gettext_lang("fr", n = 1)
    Output
      Test des fonctions `gettext()` et `gettextf()` de svBase :
      Ceci doit être traduit si la langue 'fr' est prise en charge.
      Il s'agit du message numéro 3
      Vous n'avez demandé qu'un seul item

---

    Code
      test_gettext_lang("fr", n = 2)
    Output
      Test des fonctions `gettext()` et `gettextf()` de svBase :
      Ceci doit être traduit si la langue 'fr' est prise en charge.
      Il s'agit du message numéro 3
      Vous avez demandé plusieurs items

---

    Code
      test_gettext_lang("fr_FR.UTF-8", n = 2)
    Output
      Test des fonctions `gettext()` et `gettextf()` de svBase :
      Ceci doit être traduit si la langue 'fr_FR.UTF-8' est prise en charge.
      Il s'agit du message numéro 3
      Vous avez demandé plusieurs items

