---
author: Mats Rauhala
title: Journals
---

There has been a lot of talk about programming journals lately. The common
motivation seems to be learning, by writing down what you have done when the
problems have arisen and how you solved them. I am not a learning expert, but
my take on it is that by writing down what you have done, when it's still
clearly on your mind, it forces you to shape it and process it. You process
your mangled thoughts into tangible words and sentences and thus solidify the
solution. There is also an added benefit of having a solution written down if
the problem rears its head again.

Many people do their journal with pen and paper. I can understand their
reasoning and there definitely are some great pros in writing it like that. I'm
a bit stubborn with this, I want to write it with a computer. Writing text with
a keyboard is a breeze and journals are test. Only if you find the need to
write diagrams and/or formulas would it be different. Aside from that, I don't
need to change my position, nor do I need to take my hands off the keyboard
(hey, I'm a vim user ;)). So clearly I need a tool of some sort to do the hard
work for me and this is where things get hairy.

A while back I wrote a website [introitu](http://introitu.info). The basic
problem it tries to solve overlaps with this project. I wanted a tool where I
can easily store notes and ideas as I have them. One of the ways I tried to
'market it' was specifically journals, but unfortunately as it is, it isn't
well suited for that. It's well suited for writing down my ideas, plannings,
thoughts etc. but not content that is related to time. Even combining notes to
projects isn't feasible, the topic system in it is not flexible enough. But the
good thing about this is that, after using it for a while, I can see where it
excels and where it's missing. And as it stands, no journals in there.

I have a couple of ideas how I could enhance introitu to better suit journal
type of notes. The one thing of what I'm sure right now is that I want writing
notes to be as unintrusive as possible.

1. Longer session time. If I have to spend up to 2 minutes to log in it misses
   the point of simple journal notes.
2. Simple note adding. I just want to write the note and press enter (or click
   save, depending if return is possible)

Those two are the absolute minimum requirements for me. Creating a new journal
can be a lengthier task as long as adding notes to a journal is as simple as
possible. What I really want is a command line tool that would post the note
into introitu.

So I don't have a clear idea what I need, so what then? Naturally I wrote a
mockup. A simple shell script that I've been using for a couple of weeks now.
You can find the tool from my [github
repository](https://github.com/MasseR/shellscripts). As a mockup it doesn't
deserve to have its own repository, instead it's mashed together with my other
shellscripts.

The script consists of several subcommands, but the most used one is `add`.
`add [journal] <note>` adds a new note to a specified journal. It can take the
note as the third argument or it can open the users preferred editor for a
longer text.

### Creating a new journal

You can create a new journal with the `create [journal]` command. As it is,
it's quite useless. The `add` command also creates the journal if it doesn't
exist, but having a separate `create` command gives a more clear and realistic
command structure. Internally it just creates a directory
`~/.journal/journal_name`.

This command is one possible place for enhancement. Right now the journal names
must be a single word (without whitespaces) to suit the `mkdir` requirements.
You could make the `create` command take metainformation about the journal,
like the proper name. You could then create the underlying directory where the
notes are stored from the name and then access it either by numbering the
journals (like for example todo.sh does) or providing access to the translated
directory name. The metadata could/should be saved on its own file, something
like `~/.journal/project.meta`, so that it won't interfere with reading notes.

### Adding a new note

Adding new notes is the one thing I've been doing a lot. Right now the high
level operation is that first you check the name of your journal, then add your
note with `add [journal] <note>`. The last argument, the note itself is
optional. If you do not provide the note an editor pops up. After saving and
quitting the editor the note is saved.

Internally adding notes is a somewhat complex operation. A new directory based
on date is created inside the journal directory. Inside this directory the note
is saved as `seconds.markdown`. It is highly unlikely that the user creates two
notes within the same second so this is also an easy way to allow syncronizing
between multiple devices as each note is its own separate file ^[Not entirely
true as logs are written to central files between all journals]. There won't be
a situation where one person edits two notes in two separate devices.

Each note is prefixed with a date string. Whereas the date and posix seconds
are useful internally, the date string is useful when reading your notes. In
other words, every note is automatically given a date, user only needs to
concentrate on the content.

### Reading notes

Reading notes happens with the `cat [journal]` command. This is probably the
simplest of them all. You just provide the journal name and thanks to the
internal structure, all the files can be sorted and then catenated one after
the other. Also thanks to the prefix date string, the user can keep track
easily when the note was written.

### Listing notes

Listing notes in the current implementation is by no doubt the simplest of them
all. It's so simple that it's buggy in the current implementation. It just
lists the root directory. Seeing how there are now unhidden log files, `list`
lists those too.

### Private mode

Private mode is a separate mode that affects both `add` and `cat`. Without the
private mode, only the markdown files are catenated. With the private mode the
gpg encrypted files are decrypted (user is prompted for the password) and then
catenated normally.

Adding files is basically as simple as catenating them. Without the private
mode, a markdown file is created and saved to the appropriate filename and
directory. If the private mode is set, the only difference is that the file is
saved as a gpg encrypted file.

## Further thoughts

Producing journals and notes is good enough for me (altough I have a few ideas
to enhance them), but consuming is another beast. Right now the only way to
consume the journals is catenating them and maybe piping them to pandoc for
different formats. There is no functionality for searching or filtering.

I mentioned searching and filtering, but the truth is that I don't know if
either of those is a good way. I think this is something I need to sit on, wait
for more data to come and more importantly situations to come up where I need
to read a certain note/certain notes.
