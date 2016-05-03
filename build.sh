#!/bin/bash
# Stop on any errors
set -e
BASE_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd)"

deps () {
	# Build DPDK
	$BASE_DIR/3rdparty/get-dpdk.sh
	proc="$(nproc)"
	make -j $proc -C $BASE_DIR/native

	# Build cargo
	pushd $BASE_DIR/cargo
	cargo build --release
	popd
}

if [ $# -ge 1 ]; then
	TASK=$1
else
	TASK=build
fi

case $TASK in
	help)
		echo "./build.sh <Command>
		Where command is one of
		build: Build the project
		doc: Run rustdoc and produce documentation
		fmt: Run rustfmt to format text prettily.
		lint: Run clippy to lint the project
		"
		;;
	build)
		deps
		pushd $BASE_DIR/framework
		$BASE_DIR/cargo/target/release/cargo build --release
		popd

		pushd $BASE_DIR/test/framework-test
		$BASE_DIR/cargo/target/release/cargo build --release
		popd
		
		pushd $BASE_DIR/test/delay-test
                $BASE_DIR/cargo/target/release/cargo build --release
                popd

		pushd $BASE_DIR/test/chain-test
                $BASE_DIR/cargo/target/release/cargo build --release
                popd

		pushd $BASE_DIR/test/lpm
                $BASE_DIR/cargo/target/release/cargo build --release
                popd

		pushd $BASE_DIR/test/nat
                $BASE_DIR/cargo/target/release/cargo build --release
                popd

		pushd $BASE_DIR/test/maglev
                $BASE_DIR/cargo/target/release/cargo build --release
                popd
		;;
	fmt)
		deps
		pushd $BASE_DIR/framework
		$BASE_DIR/cargo/target/release/cargo fmt
		popd

		pushd $BASE_DIR/test/framework-test
		$BASE_DIR/cargo/target/release/cargo fmt
		popd

		pushd $BASE_DIR/test/delay-test
		$BASE_DIR/cargo/target/release/cargo fmt
		popd

		pushd $BASE_DIR/test/chain-test
                $BASE_DIR/cargo/target/release/cargo build --release
                popd

		pushd $BASE_DIR/test/lpm
                $BASE_DIR/cargo/target/release/cargo build --release
                popd

		pushd $BASE_DIR/test/nat
                $BASE_DIR/cargo/target/release/cargo build --release
                popd

		pushd $BASE_DIR/test/maglev
                $BASE_DIR/cargo/target/release/cargo build --release
                popd
		;;
	doc)
		deps
		pushd $BASE_DIR/framework
		$BASE_DIR/cargo/target/release/cargo rustdoc -- \
			--no-defaults --passes "collapse-docs" --passes \
				"unindent-comments" 
		popd
		;;
	lint)
		deps
		pushd $BASE_DIR/framework
		$BASE_DIR/cargo/target/release/cargo clean; $BASE_DIR/cargo/target/release/cargo build --features dev
		popd
		;;
	clean)
		rm $BASE_DIR/3rdparty/dpdk.tar.gz || true
		rm -rf $BASE_DIR/3rdparty/dpdk || true
		make -C $BASE_DIR/native clean || true
		pushd $BASE_DIR/framework
		$BASE_DIR/cargo/target/release/cargo clean || true
		popd

		pushd $BASE_DIR/test/framework-test
		$BASE_DIR/cargo/target/release/cargo clean || true
		popd
		
		pushd $BASE_DIR/test/delay-test
                $BASE_DIR/cargo/target/release/cargo clean || true
                popd

		pushd $BASE_DIR/test/chain-test
                $BASE_DIR/cargo/target/release/cargo build --release
                popd

		pushd $BASE_DIR/test/lpm
                $BASE_DIR/cargo/target/release/cargo build --release
                popd

		pushd $BASE_DIR/test/nat
                $BASE_DIR/cargo/target/release/cargo build --release
                popd

		pushd $BASE_DIR/test/maglev
                $BASE_DIR/cargo/target/release/cargo build --release
                popd
		;;
esac
