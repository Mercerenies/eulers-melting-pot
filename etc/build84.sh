#!/bin/bash

# Run this from the root project folder (i.e. the eulers-melting-pot directory), NOT from etc/

mkdir tmp
cd tmp
cp ../problem84.e .
cat <<EOF >./problem84.ecf
<?xml version="1.0" encoding="ISO-8859-1"?>
<system xmlns="http://www.eiffel.com/developers/xml/configuration-1-20-0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.eiffel.com/developers/xml/configuration-1-20-0 http://www.eiffel.com/developers/xml/configuration-1-20-0.xsd" name="problem84" uuid="7515FD7D-99D7-4A81-B55A-8FB6A9F436ED">
  <target name="common" abstract="true">
    <root class="PROBLEM84" feature="make"/>
    <option warning="true">
      <assertions precondition="true"/>
    </option>
    <setting name="console_application" value="true"/>
    <setting name="dead_code_removal" value="all"/>
    <capability>
      <concurrency support="none"/>
      <void_safety support="none"/>
    </capability>
    <library name="base" location="\$ISE_LIBRARY\library\base\base.ecf"/>
    <library name="time" location="\$ISE_LIBRARY\library\time\time.ecf"/>
		<cluster name="root_cluster" location=".\"/>
	</target>
	<target name="classic" extends="common">
	</target>
</system>
EOF
ec -config ./problem84.ecf
(
  cd EIFGENs/classic/W_code
  finish_freezing
)
./EIFGENs/classic/W_code/problem84
cd ..
rm -r tmp
