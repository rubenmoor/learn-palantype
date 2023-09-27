# Command Keys

## Palantype command-key mode

Just like with the number mode, you can activate "Show special modes"
and hold down `N+-` to have a look at the available command keys.

![The palantype virtual keyboard in command-key mode](https://github.com/rubenmoor/learn-palantype/raw/main/cms-content/SystemDE/media/commandmode.png)

A couple of those command keys are active at this web page for navigation:
You can scroll up and down, using either `UP ⯅` and `DOWN ⯆` from the arrow keys
or using `PAGE UP ⇞` and `PAGE DOWN ⇟`.
You can navigate to the previous or next exercise using the `LEFT ⯇` and `RIGHT ⯈` arrow keys.
The key to toggle the virtual keyboard is `INSERT` or `INS` for short.
And finally, you might have used `RETURN ⏎` already.

Note that the implementation of these keys is specific to this web-site.
If you want to use these steno keys elsewhere,
you need to use the Palantype DE plover plugin.
These command keys have their own dictionary inside the plugin.

In case the symbols on the virtual are not immediately clear:
In the thumb row you have

* ESCAPE,
* TAB, actually the "tabulator key",
* SUPER, or "Windows key",
* and SPACE

You might already guess, why the windows key features again.
Like with the [Number mode](/SystemDE/TextEN/56), you have access to modifier keys:
SHIFT, CONTROL, WIN, and ALT.
However, if you want to tab the windows key,
e.g. to open the start menu on the Microsoft Windows operating system,
the modifier doesn't help.
Tabbing is now possible using the command key.

These command keys are not incredibly important for stenographic typing.
`BACKSPACE` is effectively replaced by the plover back-up chord,
`RETURN` is replaced by the plover command to create a new paragraph, `N-`.
Why should you care at all?
A lot of applications use shortcuts and take advantage of the fact
that the conventional keyboard has such an abundance of keys—
no matter if they are easy to reach for someone who care about efficient typing.

Did you know that `CTRL + SHIFT + ESC` opens the task manager in Microsoft Windows?
With plover, that would be `DʃN+U`.
In Microsoft PowerPoint (and some other programs),
`CTRL + SHIFT + UP/DOWN` is used to move a bullet point up or down the list.
Of course, having a steno mindset, you can reconfigure shortcuts of any application that you use intensively.
With the Palantype command keys, you don't have to.

### Function keys

As a bonus, hold down `G+-` to get access to the F-keys, too.

![The palantype virtual keyboard in function-key mode](https://github.com/rubenmoor/learn-palantype/raw/main/cms-content/SystemDE/media/fkeysmode.png)

Now you can reach `ALT + F4` to close an application and `F5` to reload your browser.

### Unreachable combinations

The layout for the modifier keys does not allow to combine any of `CTRL`, `ALT`, and `SUPER`.
There exists one essential key combination that thus remains unreachable:
`CTRL + ALT + DEL`.
Of course, given the fact that arbitrary key combinations can be entered in the plover dictionary,
this isn't really any strict limit.
For this particular key combination, there exists already a steno code in the command keys:

`N++NSD`

### Collisions and exclusions

Unfortunately, some of the steno codes that are implied by the command key mode
are already in use.
When a steno code has competing meanings, there is a collision that needs resolution.
Here is a list of all collisions:

| steno code   | command      | word         |
|--------------|--------------|--------------|
| A+S          | WIN + a      | aß           |
| A+SD         | WIN + A      | aßt          |
| A+ʃD         | CTRL + A     | acht         |
| E+ʃD         | CTRL + E     | echt         |
| FG+O         | ALT + F12    | quo          |
| SG+I         | WIN + F11    | Ski          |
| ʃG+O         | CTRL + F12	  | zwo          |

If there were a lot of collisions, we would have to reconsider the choice of
characters that trigger the special mode.
Luckily it's only a handful and the relatively simple scheme doesn't seem like
such a bad choice.

But still, something needs to be done.
By default, the plover software will use the first valid code it finds.
Given that the codes are found in different dictionaries, in theory, the user
could just put either one of the dictionaries on top of the list.
This would then allow to set the priority on either words or commands.
However, this will also result in unexpected behavior for the affected steno
codes in the majority of cases.

Imagine you want expect to type *echt* and you trigger some shortcut in whatever
program you happen to be using instead.
But the same goes the other way round:
Imagine you don't want to do any text input and really want to reach `CTRL + E`.
The fact that this kind of situation is unlikely to arise doesn't help;
it just makes it much harder to figure out what's going on.

The solution is to leave the word generation intact and instead exclude all of
the above steno codes from the command generation.
That means none of the commands in the list above actually work.
Or put differently: the steno codes always produce the word and never the command.
In case you want to use one of the commands, you have two options:

First, you can add your own, specific steno code to reach the command.
Second, you can (sometimes) re-configure the software in question to rely on a
different shortcut.
