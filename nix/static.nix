let
  # Takes a zlib derivation and overrides it to have both .a and .so files.
  statify_zlib = zlib_drv:
    (zlib_drv.override {
      shared = true;
      static = true;
      splitStaticOutput = false;
    }).overrideAttrs (old: { dontDisableStatic = true; });

  # Takes a curl derivation and overrides it to have both .a and .so files,
  # and have the `curl` executable be statically linked.
  statify_curl_including_exe = curl_drv: zlib_both:
    (curl_drv.override (old: {
      # Disable gss support, because that requires `krb5`, which
      # (as mentioned in note [krb5 can only be static XOR shared]) is a
      # library that cannot build both .a and .so files in its build system.
      # That means that if we enable it, we can no longer build the
      # dynamically-linked `curl` binary from the overlay
      # `archiveFilesOverlay` below where `statify_curl_including_exe` is used.
      gssSupport = false;
      zlib = zlib_both;
    })).overrideAttrs (old: {
      dontDisableStatic = true;

      # Additionally, flags to also build a static `curl` executable:

      # Note: It is important that in the eventual `libtool` invocation,
      # `-all-static` comes before (or instead of) `-static`.
      # This is because the first of them "wins setting the mode".
      # See https://lists.gnu.org/archive/html/libtool/2006-12/msg00047.html
      # libtool makes various problems with static linking.
      # Some of them are is well-described by
      #   https://github.com/sabotage-linux/sabotage/commit/57a989a2e23c9e46501da1227f371da59d212ae4
      # However, so far, `-all-static` seems to have the same effect
      # of convincing libtool to NOT drop the `-static` flag.
      # Other places where this was dicussed (in case you have to debug this in
      # the future) are:
      #   https://debbugs.gnu.org/cgi/bugreport.cgi?bug=11064
      #   https://github.com/esnet/iperf/issues/632
      # Another common thing that people do is to pass `-static --static`,
      # with the intent that `--static` isn't eaten by libtool but still
      # accepted by e.g. gcc. In our case as of writing (nixpkgs commit bc94dcf50),
      # this isn't enough. That is because:
      #   * The `--with-*=/path` options given to curl's `./configure`
      #     are usually `.lib` split outputs that contain only headers and
      #     pkg-config `.pc` files. OK so far.
      #   * For some of these, e.g. for libssh2, curl's `./configure` turns them
      #     into `LDFLAGS=-L/...libssh2-dev/lib`, which doesn't do anything to
      #     libtool, gcc or ld, because `*-dev/lib` contains only `lib/pkgconfig`
      #     and no libraries.
      #   * But for others, e.g. for libnghttp2, curl's `./configure` resolves
      #     them by taking the actual `-L` flags out of the `.pc` file, and turns
      #     them into e.g. `LDFLAGS=-L/...nghttp2-lib/lib`, which contains
      #     `{ *.la, *.a, *.so }`.
      #   * When libtool is invoked with such `LDFLAGS`, it adds two entries to
      #     `./lib/libcurl.la`'s `dependency_libs=`: `-L/...nghttp2-lib/lib` and
      #     `/...nghttp2-lib/lib/*.la`.
      #     When the `.la` path is given, libtool will read it, and pass the
      #     `.so` file referred to within as a positional argument to e.g. gcc,
      #     even when linking statically, which will result in linker error
      #         ld: attempted static link of dynamic object `/...-nghttp2-lib/lib/libnghttp2.so'
      #     I believe this is what
      #         https://github.com/sabotage-linux/sabotage/commit/57a989a2e23c9e46501da1227f371da59d212ae4
      #     fixes.
      # If we pass `-all-static` to libtool, it won't do the things in the last
      # bullet point, causing static linking to succeed.
      makeFlags = [ "curl_LDFLAGS=-all-static" ];
    });

  # Overlay that enables `.a` files for as many system packages as possible.
  # This is in *addition* to `.so` files.
  # See also https://github.com/NixOS/nixpkgs/issues/61575
  # TODO Instead of overriding each individual package manually,
  #      override them all at once similar to how `makeStaticLibraries`
  #      in `adapters.nix` does it (but without disabling shared).
in final: previous: {
    libffi = previous.libffi.overrideAttrs (old: { dontDisableStatic = true; });

    sqlite = previous.sqlite.overrideAttrs (old: { dontDisableStatic = true; });

    lzma = previous.lzma.overrideAttrs (old: { dontDisableStatic = true; });

    # Note [Packages that can't be overridden by overlays]
    # TODO: Overriding the packages mentioned here has no effect in overlays.
    #       This is because of https://github.com/NixOS/nixpkgs/issues/61682.
    #       That's why we make up new package names with `_static` at the end,
    #       and explicitly give them to packages or as linker flags in `statify`.
    #       See also that link for the total list of packages that have this problem.
    #       As of original finding it is, as per `pkgs/stdenv/linux/default.nix`:
    #           gzip bzip2 xz bash coreutils diffutils findutils gawk
    #           gnumake gnused gnutar gnugrep gnupatch patchelf
    #           attr acl zlib pcre
    acl_static = previous.acl.overrideAttrs (old: { dontDisableStatic = true; });
    attr_static = previous.attr.overrideAttrs (old: { dontDisableStatic = true; });
    bash_static = previous.bash.overrideAttrs (old: { dontDisableStatic = true; });
    bzip2_static = previous.bzip2.overrideAttrs (old: { dontDisableStatic = true; });
    coreutils_static = previous.coreutils.overrideAttrs (old: { dontDisableStatic = true; });
    diffutils_static = previous.diffutils.overrideAttrs (old: { dontDisableStatic = true; });
    findutils_static = previous.findutils.overrideAttrs (old: { dontDisableStatic = true; });
    gawk_static = previous.gawk.overrideAttrs (old: { dontDisableStatic = true; });
    gnugrep_static = previous.gnugrep.overrideAttrs (old: { dontDisableStatic = true; });
    gnumake_static = previous.gnumake.overrideAttrs (old: { dontDisableStatic = true; });
    gnupatch_static = previous.gnupatch.overrideAttrs (old: { dontDisableStatic = true; });
    gnused_static = previous.gnused.overrideAttrs (old: { dontDisableStatic = true; });
    gnutar_static = previous.gnutar.overrideAttrs (old: { dontDisableStatic = true; });
    gzip_static = previous.gzip.overrideAttrs (old: { dontDisableStatic = true; });
    patchelf_static = previous.patchelf.overrideAttrs (old: { dontDisableStatic = true; });
    pcre_static = previous.pcre.overrideAttrs (old: { dontDisableStatic = true; });
    xz_static = previous.xz.overrideAttrs (old: { dontDisableStatic = true; });
    zlib_both = statify_zlib previous.zlib;
    # Also override the original packages with a throw (which as of writing
    # has no effect) so we can know when the bug gets fixed in the future.
    # [previously there were overrides here, but they stopped working, read below]
    # For unknown reason we can't do this check on `zlib`, because if we do, we get:
    #
    #   while evaluating the attribute 'zlib_static' at /home/niklas/src/haskell/static-haskell-nix/survey/default.nix:498:5:
    #   while evaluating the attribute 'zlib.override' at /home/niklas/src/haskell/static-haskell-nix/survey/default.nix:525:5:
    #   while evaluating 'issue_61682_throw' at /home/niklas/src/haskell/static-haskell-nix/survey/default.nix:455:29, called from /home/niklas/src/haskell/static-haskell-nix/survey/default.nix:525:12:
    #   If you see this, nixpkgs #61682 has been fixed and zlib should be overridden
    #
    # So somehow, the above `zlib_static` uses *this* `zlib`, even though
    # the above uses `previous.zlib.override` and thus shouldn't see this one.
    #zlib = issue_61682_throw "zlib" previous.zlib;
    # Similarly, we don't know why these are are evaluated, but it happens for
    # https://github.com/nh2/static-haskell-nix/issues/47.
    #bzip2 = issue_61682_throw "bzip2" previous.bzip2;
    #pcre = issue_61682_throw "pcre" previous.pcre;
    # Since the update to nixpkgs master for #61 also for these,
    # see https://github.com/NixOS/nixpkgs/issues/61682#issuecomment-544215621
    #acl = issue_61682_throw "acl" previous.acl;
    #attr = issue_61682_throw "attr" previous.attr;
    #bash = issue_61682_throw "bash" previous.bash;
    #coreutils = issue_61682_throw "coreutils" previous.coreutils;
    #diffutils = issue_61682_throw "diffutils" previous.diffutils;
    #findutils = issue_61682_throw "findutils" previous.findutils;
    #gawk = issue_61682_throw "gawk" previous.gawk;
    #gnugrep = issue_61682_throw "gnugrep" previous.gnugrep;
    #gnumake = issue_61682_throw "gnumake" previous.gnumake;
    #gnupatch = issue_61682_throw "gnupatch" previous.gnupatch;
    #gnused = issue_61682_throw "gnused" previous.gnused;
    #gnutar = issue_61682_throw "gnutar" previous.gnutar;
    #gzip = issue_61682_throw "gzip" previous.gzip;
    #patchelf = issue_61682_throw "patchelf" previous.patchelf;
    #xz = issue_61682_throw "xz" previous.xz;

    postgresql = (previous.postgresql.overrideAttrs (old: { dontDisableStatic = true; })).override {
      # We need libpq, which does not need systemd,
      # and systemd doesn't currently build with musl.
      enableSystemd = false;
    };

    pixman = previous.pixman.overrideAttrs (old: { dontDisableStatic = true; });
    freetype = previous.freetype.overrideAttrs (old: { dontDisableStatic = true; });
    fontconfig = previous.fontconfig.overrideAttrs (old: {
      dontDisableStatic = true;
      configureFlags = (old.configureFlags or []) ++ [
        "--enable-static"
      ];
    });
    cairo = previous.cairo.overrideAttrs (old: { dontDisableStatic = true; });
    libpng = previous.libpng.overrideAttrs (old: { dontDisableStatic = true; });
    libpng_apng = previous.libpng_apng.overrideAttrs (old: { dontDisableStatic = true; });
    libpng12 = previous.libpng12.overrideAttrs (old: { dontDisableStatic = true; });
    libtiff = previous.libtiff.overrideAttrs (old: { dontDisableStatic = true; });
    libwebp = previous.libwebp.overrideAttrs (old: { dontDisableStatic = true; });

    expat = previous.expat.overrideAttrs (old: { dontDisableStatic = true; });

    mpfr = previous.mpfr.overrideAttrs (old: { dontDisableStatic = true; });

    gmp = previous.gmp.overrideAttrs (old: { dontDisableStatic = true; });

    gsl = previous.gsl.overrideAttrs (old: { dontDisableStatic = true; });

    libxml2 = previous.libxml2.overrideAttrs (old: { dontDisableStatic = true; });

    nettle = previous.nettle.overrideAttrs (old: { dontDisableStatic = true; });

    nghttp2 = previous.nghttp2.overrideAttrs (old: { dontDisableStatic = true; });

    libssh2 = (previous.libssh2.overrideAttrs (old: { dontDisableStatic = true; }));

    keyutils = previous.keyutils.overrideAttrs (old: { dontDisableStatic = true; });

    libxcb = previous.xorg.libxcb.overrideAttrs (old: { dontDisableStatic = true; });
    libX11 = previous.xorg.libX11.overrideAttrs (old: { dontDisableStatic = true; });
    libXau = previous.xorg.libXau.overrideAttrs (old: { dontDisableStatic = true; });
    libXcursor = previous.xorg.libXcursor.overrideAttrs (old: { dontDisableStatic = true; });
    libXdmcp = previous.xorg.libXdmcp.overrideAttrs (old: { dontDisableStatic = true; });
    libXext = previous.xorg.libXext.overrideAttrs (old: { dontDisableStatic = true; });
    libXfixes = previous.xorg.libXfixes.overrideAttrs (old: { dontDisableStatic = true; });
    libXi = previous.xorg.libXi.overrideAttrs (old: { dontDisableStatic = true; });
    libXinerama = previous.xorg.libXinerama.overrideAttrs (old: { dontDisableStatic = true; });
    libXrandr = previous.xorg.libXrandr.overrideAttrs (old: { dontDisableStatic = true; });
    libXrender = previous.xorg.libXrender.overrideAttrs (old: { dontDisableStatic = true; });
    libXScrnSaver = previous.xorg.libXScrnSaver.overrideAttrs (old: { dontDisableStatic = true; });
    libXxf86vm = previous.xorg.libXxf86vm.overrideAttrs (old: { dontDisableStatic = true; });

    SDL2 = previous.SDL2.overrideAttrs (old: { dontDisableStatic = true; });
    SDL2_gfx = previous.SDL2_gfx.overrideAttrs (old: { dontDisableStatic = true; });
    SDL2_image = previous.SDL2_image.overrideAttrs (old: { dontDisableStatic = true; });
    SDL2_mixer = previous.SDL2_mixer.overrideAttrs (old: { dontDisableStatic = true; });

    libjpeg = previous.libjpeg.override (old: { enableStatic = true; });
    libjpeg_turbo = previous.libjpeg_turbo.override (old: { enableStatic = true; });

    openblas = previous.openblas.override { enableStatic = true; };

    openssl = previous.openssl.override { static = true; };

    libsass = previous.libsass.overrideAttrs (old: { dontDisableStatic = true; });

    # Disabling kerberos support for now, as openssh's `./configure` fails to
    # detect its functions due to linker error, so the build breaks, see #68.
    openssh = previous.openssh.override { withKerberos = false; };

    krb5 = previous.krb5.override {
      # Note [krb5 can only be static XOR shared]
      # krb5 does not support building both static and shared at the same time.
      # That means *anything* on top of this overlay trying to link krb5
      # dynamically from this overlay will fail with linker errors.
      staticOnly = true;
    };

    # See comments on `statify_curl_including_exe` for the interaction with krb5!
    # As mentioned in [Packages that can't be overridden by overlays], we can't
    # override zlib to have static libs, so we have to pass in `zlib_both` explicitly
    # so that `curl` can use it.
    curl = statify_curl_including_exe previous.curl final.zlib_both;

    # `fetchurl` uses our overridden `curl` above, but `fetchurl` overrides
    # `zlib` in `curl`, see
    # https://github.com/NixOS/nixpkgs/blob/4a5c0e029ddbe89aa4eb4da7949219fe4e3f8472/pkgs/top-level/all-packages.nix#L296-L299
    # so because of [Packages that can't be overridden by overlays],
    # it will undo our `zlib` override in `curl` done above (for `curl`
    # use via `fetchurl`).
    # So we need to explicitly put our zlib into that one's curl here.
    fetchurl = previous.fetchurl;
    #.override (old: {
    #  # Can't use `zlib_both` here (infinite recursion), so we
    #  # re-`statify_zlib` `final.zlib` here (interesting that
    #  # `previous.zlib` also leads to infinite recursion at time of writing).
    #  curl = old.curl.override { zlib = statify_zlib final.zlib; };
    #});

    R = (previous.R.override {
      # R supports EITHER static or shared libs.
      static = true;
      # The Haskell package `H` depends on R, which pulls in OpenJDK,
      # which is not patched for musl support yet in nixpkgs.
      # Disable Java support for now.
      javaSupport = false;
    }).overrideAttrs (old: {
      # Testsuite newly seems to have at least one segfaulting test case.
      # Disable test suite for now; Alpine also does it:
      # https://git.alpinelinux.org/aports/tree/community/R/APKBUILD?id=e2bce14c748aacb867713cb81a91fad6e8e7f7f6#n56
      doCheck = false;
    });
}
