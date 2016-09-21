emacs ?= emacs
wget ?= wget

elpa_dir=~/.emacs.d/elpa
auto ?= vagrant-autoloads.el

el = $(filter-out $(auto),$(wildcard *.el))
elc = $(el:.el=.elc)

# emacs flags
loadpath=.
batch = $(emacs) -batch -L $(loadpath)                     \
        --eval "(let ((default-directory                   \
                      (expand-file-name \"$(elpa_dir)\"))) \
                  (normal-top-level-add-subdirs-to-load-path))"

auto_flags ?= \
        -L $(loadpath)                                               \
        --eval "(let ((generated-autoload-file                       \
                      (expand-file-name (unmsys--file-name \"$@\"))) \
                      (wd (expand-file-name default-directory))      \
                      (backup-inhibited t)                           \
                      (default-directory                             \
                      (expand-file-name \"$(elpa_dir)\")))           \
                   (normal-top-level-add-subdirs-to-load-path)       \
                   (update-directory-autoloads wd))"

.PHONY: $(auto) clean distclean
all: compile $(auto)

compile : $(elc)
%.elc : %.el
	$(batch) -f batch-byte-compile $<

$(auto):
	$(emacs) -batch $(auto_flags)

README.md: el2markdown.el vagrant.el
	$(emacs) -batch -l $< vagrant.el -f el2markdown-write-readme
	$(RM) $@~

.INTERMEDIATE: el2markdown.el
el2markdown.el:
	$(wget) -q -O $@ "https://github.com/Lindydancer/el2markdown/raw/master/el2markdown.el"

TAGS: $(el)
	$(RM) $@
	touch $@
	ls $(el) | xargs etags -a -o $@

clean:
	$(RM) *~

distclean: clean
	$(RM) *autoloads.el *loaddefs.el TAGS *.elc
