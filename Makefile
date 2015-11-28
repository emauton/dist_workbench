# Most of this Makefile depends upon Basho's work. See
#   https://github.com/basho/riak &
#   https://github.com/basho/node_package
REBAR            = rebar

OVERLAY_VARS    ?=

.PHONY: deps docs

all: deps compile

export EXOMETER_PACKAGES="(basic)"
compile:
	$(REBAR) compile

deps:
	$(REBAR) get-deps

clean: docsclean
	$(REBAR) clean

distclean: clean devclean relclean
	$(REBAR) delete-deps

rel: all relclean
	$(REBAR) generate $(OVERLAY_VARS)

lint:
	elvis rock

relclean:
	rm -rf rel/workbench

xref:
	$(REBAR) skip_deps=true xref

DIALYZER_APPS = erts kernel stdlib sasl common_test eunit tools runtime_tools \
	syntax_tools compiler debugger inets os_mon
                
PLT = $(HOME)/dialyzer/workbench_plt

plt:
	@# Regarding the initial '-', this *always* warns & we're fine with that.
	@# Cf. https://www.gnu.org/software/make/manual/html_node/Errors.html
	@# We remove some deps for being unreliable in terms of specs.
	-mkdir $(HOME)/dialyzer
	-test ! -f "$(PLT)" && ( dialyzer --build_plt --output_plt $(PLT) \
	                                  --apps $(DIALYZER_APPS)         \
	                                  -r deps                         \
	                                  --statistics --verbose ;        \
	                         dialyzer --remove_from_plt --plt $(PLT)  \
	                                  --statistics --verbose          \
	                                  -r deps/exometer deps/exometer_core deps/recon  )

dialyze: plt
	@# Some of riak_core still doesn't define -callback specs, so we ignore
	@# that warning.
	dialyzer --plt $(PLT) -Wno_undefined_callbacks \
	         --statistics --verbose apps/*/ebin

# To test a specific app / module, use e.g.
#    rebar apps=$APP skip_deps=true eunit suites=$MODULE
eunit:
	$(REBAR) skip_deps=true eunit

tests: compile xref dialyze eunit

docs: docsclean
	$(REBAR) skip_deps=true doc
	cp apps/workbench/doc/*.png apps/workbench/edoc

docsclean:
	rm -rf apps/*/edoc

stage : rel
	$(foreach dep,$(wildcard deps/*), rm -rf rel/workbench/lib/$(shell basename $(dep))-* && ln -sf $(abspath $(dep)) rel/workbench/lib;)
	$(foreach app,$(wildcard apps/*), rm -rf rel/workbench/lib/$(shell basename $(app))-* && ln -sf $(abspath $(app)) rel/workbench/lib;)


##
## Developer targets
##
##  devN - Make a dev build for node N
##  stagedevN - Make a stage dev build for node N (symlink libraries)
##  devrel - Make a dev build for 1..$DEVNODES
##  stagedevrel Make a stagedev build for 1..$DEVNODES
##
##  Example, make a 68 node devrel cluster
##    make stagedevrel DEVNODES=68

.PHONY : stagedevrel devrel
DEVNODES ?= 4

# 'seq' is not available on all *BSD, so using an alternate in awk
SEQ = $(shell awk 'BEGIN { for (i = 1; i < '$(DEVNODES)'; i++) printf("%i ", i); print i ;exit(0);}')

$(eval stagedevrel : $(foreach n,$(SEQ),stagedev$(n)))
$(eval devrel : $(foreach n,$(SEQ),dev$(n)))

dev% : all
	mkdir -p dev
	rel/gen_dev $@ rel/vars/dev_vars.config.src rel/vars/$@_vars.config
	(cd rel && $(REBAR) generate target_dir=../dev/$@ overlay_vars=vars/$@_vars.config)

stagedev% : dev%
	  $(foreach dep,$(wildcard deps/*), rm -rf dev/$^/lib/$(shell basename $(dep))* && ln -sf $(abspath $(dep)) dev/$^/lib;)
	  $(foreach app,$(wildcard apps/*), rm -rf dev/$^/lib/$(shell basename $(app))* && ln -sf $(abspath $(app)) dev/$^/lib;)

devclean: clean
	rm -rf dev
	rm -f rel/vars/dev?_vars.config

## node_package packaging config, cf. https://github.com/basho/node_package
.PHONY: package
PKG_VERSION=0.${CIRCLE_BUILD_NUM}
PKG_ID=workbench-${PKG_VERSION}
export PKG_VERSION PKG_ID REBAR OVERLAY_VARS

package.src: deps
	mkdir -p package
	rm -rf package/$(PKG_ID)
	git archive --format=tar --prefix=$(PKG_ID)/ HEAD | (cd package && tar -xf -)
	${MAKE} -C package/$(PKG_ID) deps
	mkdir -p package/$(PKG_ID)/priv
	#git --git-dir=.git describe --tags >package/$(PKG_ID)/priv/vsn.git
	for dep in package/$(PKG_ID)/deps/*; do \
             echo "Processing dep: $${dep}"; \
             mkdir -p $${dep}/priv; \
             git --git-dir=$${dep}/.git describe --tags >$${dep}/priv/vsn.git; \
        done
	find package/$(PKG_ID) -depth -name ".git" -exec rm -rf {} \;
	tar -C package -czf package/$(PKG_ID).tar.gz $(PKG_ID)

dist: package.src
	cp package/$(PKG_ID).tar.gz .

package: package.src
	${MAKE} -C package -f $(PKG_ID)/deps/node_package/Makefile

pkgclean: distclean
	rm -rf package
