Name:           time_limit
%define projectname Time_limit
%define breed -no_debug
%define debug_package %{nil}
Version:        00.09.01
Release:        K01%{?dist}
Summary:        A windowed/full-screen countdown timer.
License:        GPLv3+
URL:            https://time-limit.sourceforge.io/
Source0:        https://sourceforge.net/projects/time-limit/files/SRC/%{name}-%{version}-src%{breed}.7z
Group:     Applications/Presentation
BuildRequires:  fpc >= 3.0.0, lazarus >= 1.8.0, glib2-devel, gtk2-devel, p7zip, ImageMagick

%description
A windowed/full-screen countdown timer.
Colour changes are used as warnings.

Appearance Colour/font/time limits can be changed by setting window and hot-keys.

%prep
%setup -q -n %{version}%{breed}

%build
lazbuild Time_limit.lpi

%install
mkdir -p %{buildroot}%{_bindir}
cp %{projectname} %{buildroot}%{_bindir}/%{name}
mkdir -p ${RPM_BUILD_ROOT}%{_datadir}/locale
cd languages/
for f in $(find -iname *.mo); do
	echo $f
	tmp1=${f%.mo}
	lang=${tmp1##*.}
	mkdir -p ${RPM_BUILD_ROOT}%{_datadir}/locale/$lang/LC_MESSAGES
	cp $f $RPM_BUILD_ROOT/%{_datadir}/locale/$lang/LC_MESSAGES/%{name}.mo
done
cd ..
mkdir -p ${RPM_BUILD_ROOT}%{_docdir}/%{name}
cp README.md CHANGES LICENSE ${RPM_BUILD_ROOT}%{_docdir}/%{name}
mkdir -p ${RPM_BUILD_ROOT}%{_datadir}/applications
cat > ${RPM_BUILD_ROOT}%{_datadir}/applications/%{name}.desktop <<- "EOF"
[Desktop Entry]
Name=Time limit countdown
Name[lv]=Laika atskaites pulkstenis
Comment=A windowed/full-screen countdown timer. Use mouse right button for settings and help.
Comment[lv]=Rāda, cik laika palicis no atvēlētā. Uzziņai un izmaiņām lieto peles labo pogu.
Keywords=presentation;timer;lecture;
Exec=%{name}
Icon=%{name}
Terminal=false
Type=Application
Categories=Utility;Presentation;
EOF

mkdir PNG
mkdir -p ${RPM_BUILD_ROOT}%{_datadir}/icons/hicolor
convert Time_limit.ico PNG/time-limit.png
cd PNG
for f in *; do
	pnginfo=$(identify $f | grep -oP '.*PNG\s*\d+')
	c=${pnginfo##* }
	mkdir -p ${RPM_BUILD_ROOT}%{_datadir}/icons/hicolor/${c}x${c}/apps
	cp $f ${RPM_BUILD_ROOT}%{_datadir}/icons/hicolor/${c}x${c}/apps/%{name}.png 
done



%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(-,root,root)
%attr(755,root,root) %{_bindir}/*
%{_docdir}/%{name}
%{_datadir}/locale
%{_datadir}/applications
%{_datadir}/icons

%changelog
* Tue Jun 05 2018 Karlis Kalviskis <eko@lanet.lv> - 00.08.08 - K01
- Update to 00.08.08

* Tue May 29 2018 Karlis Kalviskis <eko@lanet.lv> - 00.08.06 - K01
- Update to 00.08.06

* Fri May 25 2018 Karlis Kalviskis <eko@lanet.lv> - 00.08.05 - K01
- Update to 00.08.05

* Fri Mar 16 2018 Karlis Kalviskis <eko@lanet.lv> - 00.08.04 - K01
- Update to 00.08.04

* Wed Mar 07 2018 Karlis Kalviskis <eko@lanet.lv> - 00.08.03 - K01
- Update to 00.08.03

* Mon Feb 19 2018 Karlis Kalviskis <eko@lanet.lv> - 00.08.00 - K01
- Update to 00.08.00

* Fri Feb 09 2018 Karlis Kalviskis <eko@lanet.lv> - 00.07.00 - K01
- Update to 00.07.00

* Wed Jan 31 2018 Karlis Kalviskis <eko@lanet.lv> - 00.06.00 - K01
- Update to 00.06.00

* Mon Jan 29 2018 Karlis Kalviskis <eko@lanet.lv> - 00.05.02 - K01
- Update to 00.05.02

* Wed Jan 03 2018 Karlis Kalviskis <eko@lanet.lv> - 00.04
- Initial build.
