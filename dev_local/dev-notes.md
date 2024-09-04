# DEV NOTES

## Todos

* reduce file size of ext folder (image etc.)
* fix R CMD CHECK

## Goals

add_image(
  type = "image",
  source = "file/path | URL | base64",
  position = list(top, bottom, etc.)
  pattern = "placeholder"
  slide_idx = c(1,2,3),
  slide_pattern = "pattern"
)

## Daily

* Figured out how to add custom information to a shape without corrupting the XML (see `dev_local/06_add_extra_info_to_shape`). Comes down to 1) adding a namespace to a parent node (e.g. to the document root) and adding a `uri` attribute to the `<p:ext>` element. Example of working XML:

```xml
<p:sp>
  <p:extLst>
    <p:ext uri="http://talkingdata.de/schema">
      <td:data>
        <td:image>
          <file path="my/path" epoch_ts="1717847331.70234" name="filename.png"/>
          <frame top="0" left="0" width="3" height="4"></frame>
        </td:image>
      </td:data>
    </p:ext>
  </p:extLst>
</p:sp>
```

Adding the namespace to `<p:ext>` instead will also work. It must be added to any of the ancestors. The root is suitable, to avoid repeating it in the document.

```xml
<p:ext uri="http://talkingdata.de/schema" xmlns:td="http://www.talkingdata.de">
```
          
* The `marquee` package was released, just when I needed it. Using ggtext to draw slides appears not future proof, as the last commit was 2 years ago.



