# text_replace verbose prints output

    Code
      text_replace(x, `@` = ">>>", verbose = 1L)
    Message
      i text_replace: 7 replacements across 1 slide
      pptx document with 2 slides
      Available layouts and their associated master(s):
    Output
                   layout       master
      1       Title Slide Office Theme
      2 Title and Content Office Theme
      3    Section Header Office Theme
      4       Two Content Office Theme
      5        Comparison Office Theme
      6        Title Only Office Theme
      7             Blank Office Theme

---

    Code
      text_replace(x2, `@` = ">>>", verbose = 2L)
    Message
      i text_replace: 7 replacements across 1 slide
        Slide 1 | "Textplatzhalter 3": "@" -> ">>>" (6x)
        Slide 1 | "Rechteck 6": "@" -> ">>>" (1x)
      pptx document with 2 slides
      Available layouts and their associated master(s):
    Output
                   layout       master
      1       Title Slide Office Theme
      2 Title and Content Office Theme
      3    Section Header Office Theme
      4       Two Content Office Theme
      5        Comparison Office Theme
      6        Title Only Office Theme
      7             Blank Office Theme

