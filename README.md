<h1>Comshell</h1>
<h2>1, Comshell</h2>
<div>=&nbsp;command&nbsp;based&nbsp;user&nbsp;interface</div>
<div>command&nbsp;based&nbsp;user&nbsp;interfaces&nbsp;(using&nbsp;keyboard,&nbsp;voice,&nbsp;gesture)&nbsp;are</div>
<div>&nbsp;&nbsp;faster,&nbsp;more&nbsp;convenient&nbsp;and&nbsp;more&nbsp;powerful,</div>
<div>&nbsp;&nbsp;than&nbsp;pointer&nbsp;based&nbsp;user&nbsp;interfaces&nbsp;(using&nbsp;mouse,&nbsp;touch,&nbsp;pen)</div>
<div>pointer&nbsp;based&nbsp;interface&nbsp;seems&nbsp;appealing&nbsp;at&nbsp;first&nbsp;sight,&nbsp;because&nbsp;of&nbsp;its&nbsp;discoverability;</div>
<div>but&nbsp;with&nbsp;simple&nbsp;uniform&nbsp;GUI&nbsp;(ie&nbsp;the&nbsp;oposite&nbsp;of&nbsp;what&nbsp;we&nbsp;see&nbsp;in&nbsp;websites),</div>
<div>&nbsp;&nbsp;there&nbsp;is&nbsp;no&nbsp;need&nbsp;for&nbsp;a&nbsp;pointer&nbsp;based&nbsp;user&nbsp;interface;</div>
<div></div>
<div>touch&nbsp;interface&nbsp;has&nbsp;an&nbsp;additional&nbsp;problem:&nbsp;interaction&nbsp;at&nbsp;a&nbsp;distance&nbsp;is&nbsp;not&nbsp;possible;</div>
<div>but&nbsp;it&nbsp;can&nbsp;still&nbsp;be&nbsp;useful&nbsp;in&nbsp;simple&nbsp;or&nbsp;special&nbsp;applications;</div>
<div></div>
<div>detection&nbsp;of&nbsp;voice&nbsp;commands&nbsp;is&nbsp;a&nbsp;relatively&nbsp;simple&nbsp;process&nbsp;(compared&nbsp;to&nbsp;general&nbsp;speech&nbsp;recognition),</div>
<div>&nbsp;&nbsp;because&nbsp;we&nbsp;only&nbsp;need&nbsp;to&nbsp;match&nbsp;against&nbsp;a&nbsp;relatively&nbsp;small&nbsp;set&nbsp;of&nbsp;commands;</div>
<div>a&nbsp;headset&nbsp;with&nbsp;near&nbsp;range&nbsp;microphone&nbsp;can&nbsp;be&nbsp;used,&nbsp;to&nbsp;exclude&nbsp;far&nbsp;away&nbsp;sound&nbsp;sources;</div>
<div>also&nbsp;it&nbsp;is&nbsp;better&nbsp;to&nbsp;put&nbsp;battery&nbsp;and&nbsp;transmitter&nbsp;of&nbsp;the&nbsp;headset&nbsp;in&nbsp;a&nbsp;separate&nbsp;unit,</div>
<div>&nbsp;&nbsp;that&nbsp;can&nbsp;be&nbsp;put&nbsp;in&nbsp;the&nbsp;pocket;</div>
<div>&nbsp;&nbsp;this&nbsp;makes&nbsp;the&nbsp;headset&nbsp;lighter&nbsp;and&nbsp;safer;</div>
<div></div>
<div>for&nbsp;those&nbsp;who&nbsp;neither&nbsp;can&nbsp;use&nbsp;all&nbsp;their&nbsp;fingers,&nbsp;nor&nbsp;can&nbsp;talk,</div>
<div>&nbsp;&nbsp;gesture&nbsp;based&nbsp;(2d)&nbsp;input&nbsp;can&nbsp;be&nbsp;implemented;</div>
<div></div>
<div><img src='./.data/keyboard.png' alt='keyboard.png'/></div>
<div>,&nbsp;two&nbsp;commas&nbsp;-&gt;&nbsp;";"</div>
<div>,&nbsp;comma&nbsp;followed&nbsp;by&nbsp;a&nbsp;letter&nbsp;-&gt;&nbsp;the&nbsp;symbol&nbsp;on&nbsp;its&nbsp;bottom&nbsp;right&nbsp;corner</div>
<div>,&nbsp;";"&nbsp;followed&nbsp;by&nbsp;"psi"&nbsp;followed&nbsp;by&nbsp;space&nbsp;or&nbsp;comma&nbsp;-&gt;&nbsp;"ψ"&nbsp;followed&nbsp;by&nbsp;one&nbsp;space&nbsp;or&nbsp;nothing</div>
<div>,&nbsp;"_ab_c"&nbsp;then&nbsp;two&nbsp;underscores&nbsp;-&gt;&nbsp;AbC&nbsp;(followed&nbsp;by&nbsp;one&nbsp;space)</div>
<div>,&nbsp;"__ab_c"&nbsp;then&nbsp;two&nbsp;underscores&nbsp;-&gt;&nbsp;__ab_c__</div>
<div><a href='https://github.com/adereth/dactyl-keyboard'>https://github.com/adereth/dactyl-keyboard</a></div>
<div>Kinesis&nbsp;Advantage&nbsp;2&nbsp;keyboard</div>
<div><a href='http://www.allthingsergo.com/the-best-ergonomic-mechanical-keyboards/'>http://www.allthingsergo.com/the-best-ergonomic-mechanical-keyboards/</a></div>
<div></div>
<div>for&nbsp;compatibility&nbsp;with&nbsp;other&nbsp;applications,&nbsp;we&nbsp;may&nbsp;still&nbsp;need&nbsp;a&nbsp;mouse,</div>
<div>&nbsp;&nbsp;and&nbsp;these&nbsp;extra&nbsp;keys&nbsp;which&nbsp;can&nbsp;be&nbsp;put&nbsp;in&nbsp;the&nbsp;middle&nbsp;of&nbsp;keyboard:</div>
<div>,&nbsp;arrow&nbsp;keys,&nbsp;"page&nbsp;up",&nbsp;"page&nbsp;down",&nbsp;"home",&nbsp;"end",&nbsp;"tab";</div>
<div>,&nbsp;"alt",&nbsp;"ctrl",&nbsp;"shift",&nbsp;"punctuations";</div>
<div>also&nbsp;we&nbsp;can&nbsp;have&nbsp;a&nbsp;row&nbsp;of&nbsp;function&nbsp;keys,&nbsp;plus&nbsp;the&nbsp;"delete"&nbsp;key,&nbsp;at&nbsp;the&nbsp;top;</div>
<div></div>
<div>=&nbsp;Comshell</div>
<div>Comshell&nbsp;is&nbsp;a&nbsp;unified&nbsp;computing&nbsp;environment,&nbsp;utilizing&nbsp;command&nbsp;based&nbsp;user&nbsp;interface;</div>
<div></div>
<div>project&nbsp;directories&nbsp;reside&nbsp;in&nbsp;directories&nbsp;named&nbsp;"projects"&nbsp;or&nbsp;"projects.*",</div>
<div>&nbsp;&nbsp;inside&nbsp;home&nbsp;directory&nbsp;or&nbsp;mounted&nbsp;disks;</div>
<div>list&nbsp;of&nbsp;all&nbsp;projects&nbsp;will&nbsp;appear&nbsp;in&nbsp;a&nbsp;floating&nbsp;layer,&nbsp;at&nbsp;the&nbsp;center&nbsp;of&nbsp;screen;</div>
<div>each&nbsp;group&nbsp;of&nbsp;projects&nbsp;(which&nbsp;are&nbsp;in&nbsp;the&nbsp;same&nbsp;directory)&nbsp;will&nbsp;be&nbsp;shown&nbsp;in&nbsp;separate&nbsp;tabs;</div>
<div></div>
<div>in&nbsp;a&nbsp;project&nbsp;view,&nbsp;list&nbsp;of&nbsp;files&nbsp;will&nbsp;be&nbsp;displayed&nbsp;in&nbsp;the&nbsp;left&nbsp;side_bar;</div>
<div>opened&nbsp;files&nbsp;are&nbsp;indicated&nbsp;by&nbsp;a&nbsp;line&nbsp;below&nbsp;them;</div>
<div>multiple&nbsp;views&nbsp;of&nbsp;a&nbsp;file&nbsp;are&nbsp;indicated&nbsp;by&nbsp;sections&nbsp;in&nbsp;this&nbsp;line;</div>
<div>files&nbsp;and&nbsp;directories&nbsp;with&nbsp;names&nbsp;starting&nbsp;with&nbsp;a&nbsp;dot,&nbsp;will&nbsp;be&nbsp;hidden;</div>
<div>".cache"&nbsp;directory&nbsp;inside&nbsp;a&nbsp;project&nbsp;is&nbsp;for&nbsp;files&nbsp;we&nbsp;don't&nbsp;want&nbsp;to&nbsp;share&nbsp;or&nbsp;backup;</div>
<div></div>
<div>text&nbsp;files&nbsp;will&nbsp;be&nbsp;opened&nbsp;in&nbsp;a&nbsp;text&nbsp;editor;</div>
<div>directories&nbsp;with&nbsp;".g"&nbsp;suffixes,&nbsp;will&nbsp;be&nbsp;opened&nbsp;in&nbsp;a&nbsp;gallery&nbsp;view;</div>
<div>files&nbsp;and&nbsp;directories&nbsp;inside&nbsp;a&nbsp;gallery,&nbsp;will&nbsp;be&nbsp;opened&nbsp;in&nbsp;a&nbsp;floating&nbsp;layer;</div>
<div></div>
<div>non_local&nbsp;projects,&nbsp;web&nbsp;pages,&nbsp;PDF&nbsp;documents,&nbsp;etc,</div>
<div>&nbsp;&nbsp;accessed&nbsp;from&nbsp;links&nbsp;inside&nbsp;the&nbsp;main&nbsp;project,&nbsp;will&nbsp;be&nbsp;opened&nbsp;in&nbsp;a&nbsp;floating&nbsp;layer;</div>
<div>web&nbsp;pages:</div>
<div>,&nbsp;move&nbsp;caret&nbsp;between&nbsp;visual&nbsp;objects&nbsp;(ignoring&nbsp;structural&nbsp;objects);</div>
<div>,&nbsp;or&nbsp;use&nbsp;hinting&nbsp;for&nbsp;text&nbsp;input&nbsp;and&nbsp;other&nbsp;widgets,&nbsp;text&nbsp;and&nbsp;other&nbsp;elements&nbsp;like&nbsp;images&nbsp;and&nbsp;videos;</div>
<div></div>
<div>modal&nbsp;key_bindings;</div>
<div>modes&nbsp;(normal&nbsp;mode&nbsp;and&nbsp;insert&nbsp;mode)&nbsp;must&nbsp;be&nbsp;visually&nbsp;distinctive;</div>
<div>press&nbsp;"esc"&nbsp;or&nbsp;"tab"&nbsp;to&nbsp;go&nbsp;to&nbsp;normal&nbsp;mode;</div>
<div>in&nbsp;normal&nbsp;mode&nbsp;we&nbsp;can:</div>
<div>,&nbsp;press&nbsp;"enter"&nbsp;to&nbsp;go&nbsp;to&nbsp;insert&nbsp;mode;</div>
<div>,&nbsp;move&nbsp;the&nbsp;cursor&nbsp;to&nbsp;the&nbsp;next&nbsp;or&nbsp;previous&nbsp;word;</div>
<div>,&nbsp;move&nbsp;the&nbsp;cursor&nbsp;to&nbsp;the&nbsp;next&nbsp;or&nbsp;previous&nbsp;lines&nbsp;or&nbsp;table&nbsp;cells;</div>
<div>,&nbsp;move&nbsp;the&nbsp;cursor&nbsp;to&nbsp;the&nbsp;next&nbsp;or&nbsp;previous&nbsp;paragraph;</div>
<div>,&nbsp;start&nbsp;and&nbsp;end&nbsp;selection,&nbsp;then&nbsp;copy&nbsp;or&nbsp;cut;</div>
<div>,&nbsp;paste</div>
<div>,&nbsp;undo</div>
<div>,&nbsp;find</div>
<div></div>
<div>,&nbsp;navigation:&nbsp;move,&nbsp;search</div>
<div>,&nbsp;selection</div>
<div>,&nbsp;completion</div>
<div></div>
<div>double&nbsp;space:</div>
<div>,&nbsp;at&nbsp;the&nbsp;beginning&nbsp;of&nbsp;line:&nbsp;indent</div>
<div>,&nbsp;otherwise:&nbsp;complete&nbsp;(auto_completion&nbsp;does&nbsp;not&nbsp;disappear&nbsp;with&nbsp;only&nbsp;one&nbsp;space)</div>
<h2>2, computers</h2>
<div>=&nbsp;asynchronous&nbsp;digital&nbsp;circuits</div>
<div>in&nbsp;conventional&nbsp;digital&nbsp;circuits&nbsp;when&nbsp;the&nbsp;inputs&nbsp;change,</div>
<div>&nbsp;&nbsp;the&nbsp;outputs&nbsp;can&nbsp;have&nbsp;temporary&nbsp;invalid&nbsp;values,&nbsp;until&nbsp;they&nbsp;stabilize&nbsp;to&nbsp;the&nbsp;valid&nbsp;values;</div>
<div>but&nbsp;for&nbsp;the&nbsp;circuit&nbsp;to&nbsp;do&nbsp;its&nbsp;job,</div>
<div>&nbsp;&nbsp;gates&nbsp;with&nbsp;memory&nbsp;(registers)&nbsp;must&nbsp;operate&nbsp;only&nbsp;when&nbsp;the&nbsp;inputs&nbsp;have&nbsp;correct&nbsp;values;</div>
<div>one&nbsp;solution&nbsp;is&nbsp;to&nbsp;synchronize&nbsp;registers&nbsp;with&nbsp;a&nbsp;global&nbsp;clock&nbsp;signal;</div>
<div>&nbsp;&nbsp;the&nbsp;period&nbsp;of&nbsp;clock&nbsp;signal&nbsp;is&nbsp;made&nbsp;long&nbsp;enough&nbsp;for&nbsp;the&nbsp;circuit&nbsp;to&nbsp;become&nbsp;stable;</div>
<div></div>
<div>disadvantages&nbsp;of&nbsp;synchronous&nbsp;circuits:</div>
<div>,&nbsp;we&nbsp;have&nbsp;to&nbsp;split&nbsp;long&nbsp;operations&nbsp;into&nbsp;several&nbsp;smaller&nbsp;ones,</div>
<div>&nbsp;&nbsp;which&nbsp;can&nbsp;be&nbsp;performed&nbsp;in&nbsp;successive&nbsp;clock&nbsp;cycles&nbsp;(a&nbsp;technique&nbsp;known&nbsp;as&nbsp;pipelining);</div>
<div>&nbsp;&nbsp;otherwise&nbsp;the&nbsp;circuit&nbsp;would&nbsp;be&nbsp;slow&nbsp;and&nbsp;inefficient;</div>
<div>,&nbsp;distributing&nbsp;a&nbsp;high_fan_out,&nbsp;timing_sensitive&nbsp;clock&nbsp;signal&nbsp;can&nbsp;be&nbsp;complicated;</div>
<div>,&nbsp;electromagnetic&nbsp;interference&nbsp;at&nbsp;the&nbsp;clock&nbsp;frequency&nbsp;and&nbsp;its&nbsp;harmonics;</div>
<div>,&nbsp;widely&nbsp;distributed&nbsp;clock&nbsp;signal&nbsp;takes&nbsp;a&nbsp;lot&nbsp;of&nbsp;power,</div>
<div>&nbsp;&nbsp;and&nbsp;must&nbsp;run&nbsp;whether&nbsp;the&nbsp;circuit&nbsp;is&nbsp;receiving&nbsp;inputs&nbsp;or&nbsp;not;</div>
<div>although&nbsp;"clock&nbsp;gating"&nbsp;can&nbsp;help&nbsp;to&nbsp;reduce&nbsp;some&nbsp;of&nbsp;the&nbsp;problems&nbsp;of&nbsp;synchronous&nbsp;circuits,</div>
<div>&nbsp;i&nbsp;think&nbsp;the&nbsp;real&nbsp;solution&nbsp;is&nbsp;to&nbsp;use&nbsp;asynchronous&nbsp;circuits;</div>
<div></div>
<div>the&nbsp;only&nbsp;sane&nbsp;kind&nbsp;of&nbsp;asynchronous&nbsp;circuit&nbsp;which&nbsp;i&nbsp;could&nbsp;imagine&nbsp;is&nbsp;this:</div>
<div>,&nbsp;next&nbsp;to&nbsp;any&nbsp;data&nbsp;wire&nbsp;there&nbsp;is&nbsp;a&nbsp;control&nbsp;wire&nbsp;which&nbsp;determines&nbsp;if&nbsp;the&nbsp;data&nbsp;is&nbsp;valid&nbsp;or&nbsp;not;</div>
<div>,&nbsp;when&nbsp;a&nbsp;register&nbsp;wants&nbsp;to&nbsp;change&nbsp;its&nbsp;outputs,&nbsp;it&nbsp;first&nbsp;invalidates&nbsp;them,</div>
<div>&nbsp;&nbsp;for&nbsp;a&nbsp;duration&nbsp;equal&nbsp;to&nbsp;the&nbsp;delay&nbsp;of&nbsp;one&nbsp;gate;</div>
<div>,&nbsp;any&nbsp;gate&nbsp;receiving&nbsp;an&nbsp;invalid&nbsp;input,&nbsp;invalidates&nbsp;its&nbsp;outputs;</div>
<div>,&nbsp;this&nbsp;way&nbsp;all&nbsp;data&nbsp;which&nbsp;is&nbsp;going&nbsp;to&nbsp;change&nbsp;in&nbsp;the&nbsp;future,&nbsp;is&nbsp;first&nbsp;invalidated;</div>
<div>,&nbsp;registers&nbsp;operate&nbsp;only&nbsp;when&nbsp;all&nbsp;inputs&nbsp;are&nbsp;valid;</div>
<div></div>
<div>=&nbsp;computers</div>
<div>cpu,&nbsp;memory,&nbsp;peripherals,</div>
<div>&nbsp;&nbsp;this&nbsp;seems&nbsp;to&nbsp;be&nbsp;the&nbsp;only&nbsp;practical&nbsp;architecture&nbsp;for&nbsp;the&nbsp;hardware&nbsp;of&nbsp;computers;</div>
<div>cpu&nbsp;runs&nbsp;a&nbsp;sequence&nbsp;of&nbsp;simple&nbsp;computations,&nbsp;called&nbsp;instruction&nbsp;codes,&nbsp;one&nbsp;by&nbsp;one;</div>
<div></div>
<div>compilers&nbsp;are&nbsp;special&nbsp;programs&nbsp;that&nbsp;generate&nbsp;instruction&nbsp;codes,</div>
<div>&nbsp;&nbsp;from&nbsp;a&nbsp;program&nbsp;written&nbsp;in&nbsp;a&nbsp;structured&nbsp;and&nbsp;human&nbsp;readable&nbsp;language;</div>
<div></div>
<div>there&nbsp;is&nbsp;always&nbsp;possibility&nbsp;of&nbsp;backdoors&nbsp;for&nbsp;closed&nbsp;source&nbsp;CPU,</div>
<div>&nbsp;&nbsp;especially&nbsp;if&nbsp;the&nbsp;boot&nbsp;firmware&nbsp;is&nbsp;also&nbsp;closed&nbsp;source;</div>
<div>recently&nbsp;introduction&nbsp;of&nbsp;"secure&nbsp;execution&nbsp;environment"&nbsp;makes&nbsp;this&nbsp;situation&nbsp;even&nbsp;worse;</div>
<div>it's&nbsp;a&nbsp;closed&nbsp;source,&nbsp;full_blown,&nbsp;always_running&nbsp;mini&nbsp;operating&nbsp;system,</div>
<div>&nbsp;&nbsp;with&nbsp;full&nbsp;access&nbsp;to&nbsp;the&nbsp;whole&nbsp;system&nbsp;(including&nbsp;memory&nbsp;and&nbsp;network);</div>
<div>furthermore&nbsp;they&nbsp;have&nbsp;made&nbsp;it&nbsp;practically&nbsp;impossible&nbsp;for&nbsp;users&nbsp;to&nbsp;disable&nbsp;it;</div>
<div>this&nbsp;mess&nbsp;of&nbsp;a&nbsp;design&nbsp;cries&nbsp;out&nbsp;for&nbsp;hidden&nbsp;and&nbsp;quite&nbsp;sophisticated&nbsp;backdoors;</div>
<div><a href='https://www.fsf.org/blogs/licensing/intel-me-and-why-we-should-get-rid-of-me'>https://www.fsf.org/blogs/licensing/intel-me-and-why-we-should-get-rid-of-me</a></div>
<div><a href='https://libreboot.org/faq.html#intel'>https://libreboot.org/faq.html#intel</a></div>
<div><a href='https://en.wikipedia.org/wiki/Intel_Management_Engine'>https://en.wikipedia.org/wiki/Intel_Management_Engine</a></div>
<div><a href='https://blog.invisiblethings.org/papers/2015/x86_harmful.pdf'>https://blog.invisiblethings.org/papers/2015/x86_harmful.pdf</a></div>
<div></div>
<div>one&nbsp;read_only&nbsp;ROM,&nbsp;plus&nbsp;a&nbsp;writable&nbsp;ROM:</div>
<div>,&nbsp;no&nbsp;possibility&nbsp;of&nbsp;bricking&nbsp;the&nbsp;device;</div>
<div>,&nbsp;no&nbsp;need&nbsp;for&nbsp;complex&nbsp;signing&nbsp;mechanism&nbsp;to&nbsp;make&nbsp;sure&nbsp;a&nbsp;device's&nbsp;firmware&nbsp;is&nbsp;not&nbsp;malicious;</div>
<div>&nbsp;&nbsp;just&nbsp;clear&nbsp;the&nbsp;writable&nbsp;flash&nbsp;when&nbsp;you&nbsp;buy&nbsp;a&nbsp;device;</div>
<div></div>
<div>on&nbsp;X86&nbsp;architecture&nbsp;we&nbsp;can&nbsp;have&nbsp;open&nbsp;source&nbsp;boot_loader,&nbsp;and&nbsp;GPU&nbsp;drivers,</div>
<div>&nbsp;&nbsp;using&nbsp;Intel&nbsp;with&nbsp;Coreboot+Grub2&nbsp;for&nbsp;example;</div>
<div>&nbsp;&nbsp;<a href='https://www.coreboot.org/GRUB2#Scanning_for_grub.cfg_on_local_hard_drives'>https://www.coreboot.org/GRUB2#Scanning_for_grub.cfg_on_local_hard_drives</a></div>
<div>&nbsp;&nbsp;though&nbsp;there&nbsp;sill&nbsp;will&nbsp;be&nbsp;closed&nbsp;source&nbsp;parts&nbsp;(Intel&nbsp;FSP);</div>
<div>but&nbsp;there&nbsp;is&nbsp;no&nbsp;easy&nbsp;way&nbsp;to&nbsp;get&nbsp;rid&nbsp;of&nbsp;Intel&nbsp;ME&nbsp;(or&nbsp;AMD&nbsp;PSP);</div>
<div>though&nbsp;there&nbsp;are&nbsp;some&nbsp;hacks&nbsp;for&nbsp;disabling&nbsp;(but&nbsp;not&nbsp;completely&nbsp;remove)&nbsp;Intel&nbsp;ME:</div>
<div>&nbsp;&nbsp;<a href='https://puri.sm/posts/deep-dive-into-intel-me-disablement/'>https://puri.sm/posts/deep-dive-into-intel-me-disablement/</a></div>
<div></div>
<div>ARM&nbsp;architecture&nbsp;is&nbsp;closed&nbsp;source&nbsp;too,&nbsp;but&nbsp;we&nbsp;can&nbsp;have&nbsp;open&nbsp;source&nbsp;GPU&nbsp;driver&nbsp;(Qualcomm/Adreno),</div>
<div>&nbsp;&nbsp;and&nbsp;Coreboot+Grub2&nbsp;bootloader;</div>
<div>ARM&nbsp;TrustZone&nbsp;can&nbsp;host&nbsp;an&nbsp;open&nbsp;source&nbsp;operating&nbsp;system&nbsp;too,&nbsp;apparently;</div>
<div><a href='https://news.ycombinator.com/item?id=17783357'>https://news.ycombinator.com/item?id=17783357</a></div>
<div></div>
<div>open&nbsp;source&nbsp;CPU:</div>
<div>,&nbsp;RISC_V:&nbsp;no&nbsp;adequate&nbsp;hardware&nbsp;available&nbsp;yet;</div>
<div>,&nbsp;PowerPc:&nbsp;no&nbsp;adequate&nbsp;hardware&nbsp;available&nbsp;yet;</div>
<div>,&nbsp;MIPS:&nbsp;bad&nbsp;old&nbsp;design&nbsp;(relative&nbsp;to&nbsp;other&nbsp;alternatives,&nbsp;not&nbsp;X86),</div>
<div>&nbsp;&nbsp;no&nbsp;suitable&nbsp;hardware&nbsp;available&nbsp;yet;</div>
<div></div>
<div>programs&nbsp;usually&nbsp;do&nbsp;not&nbsp;run&nbsp;directly&nbsp;on&nbsp;computer&nbsp;hardware;</div>
<div>instead&nbsp;they&nbsp;run&nbsp;on&nbsp;a&nbsp;more&nbsp;sophisticated&nbsp;software&nbsp;machine&nbsp;(a&nbsp;virtual&nbsp;machine)&nbsp;called&nbsp;the&nbsp;kernel;</div>
<div>in&nbsp;theory&nbsp;we&nbsp;can&nbsp;live&nbsp;without&nbsp;a&nbsp;kernel&nbsp;(an&nbsp;idea&nbsp;sometimes&nbsp;called&nbsp;a&nbsp;library&nbsp;operating&nbsp;system);</div>
<div>but&nbsp;in&nbsp;that&nbsp;case,&nbsp;we&nbsp;have&nbsp;to&nbsp;rewrite&nbsp;all&nbsp;the&nbsp;required&nbsp;libraries,&nbsp;on&nbsp;bare&nbsp;metal;</div>
<div>anyway,&nbsp;having&nbsp;an&nbsp;operating&nbsp;system,&nbsp;makes&nbsp;developing&nbsp;and&nbsp;testing&nbsp;new&nbsp;programs&nbsp;much&nbsp;easier;</div>
<div></div>
<div>Linux&nbsp;is&nbsp;a&nbsp;highly&nbsp;developed,&nbsp;constantly&nbsp;evolving,&nbsp;open_source&nbsp;kernel;</div>
<div>in&nbsp;Linux&nbsp;(and&nbsp;other&nbsp;Unix&nbsp;based&nbsp;operating&nbsp;systems)&nbsp;most&nbsp;things&nbsp;appear&nbsp;in&nbsp;the&nbsp;file&nbsp;system;</div>
<div>i&nbsp;think&nbsp;the&nbsp;reason&nbsp;is&nbsp;to&nbsp;make&nbsp;it&nbsp;possible&nbsp;to&nbsp;do&nbsp;lots&nbsp;of&nbsp;things&nbsp;using&nbsp;shell&nbsp;scripts,</div>
<div>&nbsp;&nbsp;instead&nbsp;of&nbsp;a&nbsp;proper&nbsp;programming&nbsp;language;</div>
<div>while&nbsp;i&nbsp;can&nbsp;understand&nbsp;the&nbsp;convenience&nbsp;it&nbsp;provides,&nbsp;i&nbsp;don't&nbsp;think&nbsp;it's&nbsp;good&nbsp;design;</div>
<h2>3, ArchLinux</h2>
<div>using&nbsp;Arch&nbsp;Linux&nbsp;we&nbsp;can&nbsp;easily&nbsp;setup&nbsp;and&nbsp;maintain&nbsp;a&nbsp;Linux&nbsp;system;</div>
<div>the&nbsp;following&nbsp;shows&nbsp;how&nbsp;to&nbsp;setup&nbsp;a&nbsp;basic&nbsp;graphical&nbsp;environment,&nbsp;using&nbsp;GnomeShell;</div>
<div><img src='./.data/gnome-shell.png' alt='gnome-shell.png'/></div>
<div></div>
<div>in&nbsp;the&nbsp;installed&nbsp;system,&nbsp;there&nbsp;is&nbsp;only&nbsp;one&nbsp;application,&nbsp;a&nbsp;terminal&nbsp;emulator;</div>
<div>other&nbsp;applications&nbsp;can&nbsp;be&nbsp;installed&nbsp;using&nbsp;"pacman";</div>
<div>you&nbsp;can&nbsp;press&nbsp;"alt-space"&nbsp;to&nbsp;show&nbsp;the&nbsp;list&nbsp;of&nbsp;applications;</div>
<div>selecting&nbsp;an&nbsp;application&nbsp;from&nbsp;the&nbsp;list&nbsp;shows&nbsp;it&nbsp;in&nbsp;a&nbsp;dedicated&nbsp;workspace;</div>
<div>also&nbsp;you&nbsp;can&nbsp;power&nbsp;off,&nbsp;reboot,&nbsp;logout,&nbsp;suspend,&nbsp;or&nbsp;lock&nbsp;the&nbsp;system,</div>
<div>&nbsp;&nbsp;from&nbsp;the&nbsp;applications&nbsp;list,&nbsp;just&nbsp;by&nbsp;typing&nbsp;those&nbsp;commands;</div>
<div>press&nbsp;"alt-enter"&nbsp;to&nbsp;open&nbsp;a&nbsp;terminal&nbsp;window;</div>
<div>press&nbsp;"alt-esc"&nbsp;to&nbsp;close&nbsp;the&nbsp;focused&nbsp;window;</div>
<div>press&nbsp;"alt-tab"&nbsp;to&nbsp;switch&nbsp;between&nbsp;recent&nbsp;workspaces;</div>
<div>press&nbsp;"alt-a"&nbsp;to&nbsp;switch&nbsp;between&nbsp;the&nbsp;windows&nbsp;of&nbsp;a&nbsp;workspace;</div>
<div>press&nbsp;"alt-shift-space"&nbsp;to&nbsp;toggle&nbsp;maximized&nbsp;state;</div>
<div>press&nbsp;"alt-shift-s"&nbsp;or&nbsp;"alt-shift-r"&nbsp;to&nbsp;take&nbsp;a&nbsp;screen_shot&nbsp;or&nbsp;record&nbsp;a&nbsp;screen_cast;</div>
<div></div>
<div>boot&nbsp;Arch&nbsp;Linux&nbsp;live&nbsp;environment;</div>
<div>to&nbsp;ensure&nbsp;the&nbsp;system&nbsp;clock&nbsp;is&nbsp;accurate:</div>
<div>;&nbsp;timedatectl&nbsp;set-ntp&nbsp;true</div>
<div>if&nbsp;you&nbsp;need&nbsp;to&nbsp;connect&nbsp;to&nbsp;a&nbsp;WIFI&nbsp;network:</div>
<div>;&nbsp;iwctl</div>
<div>&nbsp;&nbsp;device&nbsp;list</div>
<div>&nbsp;&nbsp;station&nbsp;&lt;device&gt;&nbsp;get-networks</div>
<div>&nbsp;&nbsp;station&nbsp;&lt;device&gt;&nbsp;connect&nbsp;&lt;SSID&gt;</div>
<div></div>
<div>list&nbsp;available&nbsp;block&nbsp;devices&nbsp;using&nbsp;"lsblk",&nbsp;then&nbsp;on&nbsp;the&nbsp;intended&nbsp;block&nbsp;device,</div>
<div>&nbsp;&nbsp;create&nbsp;the&nbsp;needed&nbsp;partitions,&nbsp;and&nbsp;finally&nbsp;format&nbsp;and&nbsp;mount&nbsp;them:</div>
<div>;&nbsp;printf&nbsp;"label:&nbsp;gpt\n,260MiB,U,*\n;"&nbsp;|&nbsp;sfdisk&nbsp;/dev/&lt;device&gt;</div>
<div>;&nbsp;mkfs.fat&nbsp;-F32&nbsp;/dev/&lt;partition1&gt;;&nbsp;mkfs.btrfs&nbsp;/dev/&lt;partition2&gt;</div>
<div>;&nbsp;mount&nbsp;/dev/&lt;partition2&gt;&nbsp;/mnt</div>
<div>;&nbsp;mkdir&nbsp;-p&nbsp;/mnt/boot/efi</div>
<div>;&nbsp;mount&nbsp;/dev/&lt;partition1&gt;&nbsp;/mnt/boot/efi</div>
<div>;&nbsp;mkdir&nbsp;/mnt/etc</div>
<div>;&nbsp;genfstab&nbsp;-U&nbsp;/mnt&nbsp;&gt;&gt;&nbsp;/mnt/etc/fstab</div>
<div></div>
<div>;sh</div>
<div>pacstrap&nbsp;/mnt&nbsp;base</div>
<div>arch-chroot&nbsp;/mnt</div>
<div>curl&nbsp;https://damoonsaghian/Comshell/archive/master.tar.gz&nbsp;|&nbsp;tar&nbsp;-xz</div>
<div>cd&nbsp;Comshell/ArchLinux</div>
<div>;</div>
<div>ensure&nbsp;that&nbsp;the&nbsp;files&nbsp;aren't&nbsp;malicious,&nbsp;then:</div>
<div>;&nbsp;sh&nbsp;install.sh</div>
<div>;&nbsp;exit;&nbsp;reboot</div>
<div></div>
<div>to&nbsp;connect&nbsp;to&nbsp;a&nbsp;WIFI&nbsp;network:</div>
<div>;&nbsp;nmcli&nbsp;dev&nbsp;wifi</div>
<div>;&nbsp;nmcli&nbsp;--ask&nbsp;dev&nbsp;wifi&nbsp;con&nbsp;&lt;ssid&gt;</div>
<div>to&nbsp;disconnect&nbsp;from&nbsp;a&nbsp;WIFI&nbsp;network:</div>
<div>;&nbsp;nmcli&nbsp;con&nbsp;down&nbsp;id&nbsp;&lt;ssid&gt;</div>
<div></div>
<div>if&nbsp;your&nbsp;combined&nbsp;headset&nbsp;jack&nbsp;is&nbsp;not&nbsp;detected&nbsp;correctly,&nbsp;you&nbsp;can&nbsp;try&nbsp;this:</div>
<div>;&nbsp;pkexec&nbsp;echo&nbsp;'options&nbsp;snd_hda_intel&nbsp;index=0&nbsp;model=dell-headset-multi'&nbsp;&gt;&nbsp;/etc/modprobe.d/alsa-base.conf</div>
<div><a href='https://wiki.archlinux.org/index.php/Advanced_Linux_Sound_Architecture#Correctly_detect_microphone_plugged_in_a_4-pin_3.5mm_(TRRS)_jack'>https://wiki.archlinux.org/index.php/Advanced_Linux_Sound_Architecture#Correctly_detect_microphone_plugged_in_a_4-pin_3.5mm_(TRRS)_jack</a></div>
<div>this&nbsp;made&nbsp;the&nbsp;microphone&nbsp;available,&nbsp;but&nbsp;with&nbsp;a&nbsp;very&nbsp;bad&nbsp;static&nbsp;noise;</div>
<div>so&nbsp;maybe&nbsp;it's&nbsp;better&nbsp;to&nbsp;use&nbsp;a&nbsp;USB/Bluetooth&nbsp;sound&nbsp;card;</div>
<div></div>
<div>you&nbsp;can&nbsp;set&nbsp;"user1"&nbsp;for&nbsp;automatic&nbsp;login&nbsp;(using&nbsp;the&nbsp;root&nbsp;terminal):</div>
<div>;&nbsp;pkexec&nbsp;nano&nbsp;/etc/gdm/custom.conf</div>
<div>&nbsp;&nbsp;[daemon]</div>
<div>&nbsp;&nbsp;AutomaticLoginEnable=True</div>
<div>&nbsp;&nbsp;AutomaticLogin=user1</div>
<div>but&nbsp;to&nbsp;protect&nbsp;the&nbsp;computer&nbsp;from&nbsp;physical&nbsp;attacks,&nbsp;you&nbsp;have&nbsp;to&nbsp;disable&nbsp;automatic&nbsp;login,</div>
<div>&nbsp;&nbsp;and&nbsp;lock&nbsp;the&nbsp;session&nbsp;when&nbsp;you&nbsp;leave&nbsp;the&nbsp;computer;</div>
<div>in&nbsp;addition&nbsp;you&nbsp;have&nbsp;to:</div>
<div>,&nbsp;somehow&nbsp;prevent&nbsp;tampering&nbsp;with&nbsp;hardware;</div>
<div>,&nbsp;disable&nbsp;boot&nbsp;from&nbsp;USB&nbsp;(and&nbsp;other&nbsp;external&nbsp;ports);</div>
<div>,&nbsp;protect&nbsp;boot&nbsp;firmware&nbsp;by&nbsp;a&nbsp;password;</div>
<div>these&nbsp;can&nbsp;make&nbsp;physical&nbsp;attacks&nbsp;more&nbsp;difficult,&nbsp;but&nbsp;keep&nbsp;in&nbsp;mind&nbsp;that</div>
<div>&nbsp;&nbsp;physical&nbsp;access&nbsp;to&nbsp;a&nbsp;computer&nbsp;is&nbsp;root&nbsp;access,&nbsp;given&nbsp;enough&nbsp;time&nbsp;and&nbsp;resources;</div>
<div></div>
<div>=&nbsp;system&nbsp;administration</div>
<div>in&nbsp;system&nbsp;administration&nbsp;we&nbsp;must&nbsp;ensure&nbsp;that:</div>
<div>1,&nbsp;the&nbsp;command&nbsp;executed&nbsp;is&nbsp;explicitly&nbsp;given&nbsp;by&nbsp;the&nbsp;user;</div>
<div>2,&nbsp;if&nbsp;a&nbsp;program&nbsp;steals&nbsp;a&nbsp;"wheel"&nbsp;user's&nbsp;password,&nbsp;it&nbsp;can't&nbsp;change&nbsp;the&nbsp;system;</div>
<div></div>
<div>"sudo"&nbsp;fails&nbsp;in&nbsp;both&nbsp;cases;</div>
<div><a href='https://www.reddit.com/r/linuxquestions/comments/8mlil7/whats_the_point_of_the_sudo_password_prompt_if/'>https://www.reddit.com/r/linuxquestions/comments/8mlil7/whats_the_point_of_the_sudo_password_prompt_if/</a></div>
<div></div>
<div>"pkexec"&nbsp;is&nbsp;safer,&nbsp;but&nbsp;since&nbsp;the&nbsp;Polkit&nbsp;agent&nbsp;doesn't&nbsp;show&nbsp;command&nbsp;arguments,</div>
<div>&nbsp;&nbsp;it&nbsp;can&nbsp;be&nbsp;vulnerable&nbsp;too;</div>
<div>further&nbsp;more,&nbsp;"pkexec"&nbsp;fails&nbsp;in&nbsp;the&nbsp;second&nbsp;case;</div>
<div>&nbsp;&nbsp;a&nbsp;fake&nbsp;Polkit&nbsp;agent&nbsp;can&nbsp;be&nbsp;used&nbsp;to&nbsp;send&nbsp;the&nbsp;stolen&nbsp;password&nbsp;to&nbsp;Polkit;</div>
<div>and&nbsp;since&nbsp;Polkit&nbsp;admin&nbsp;is&nbsp;installed&nbsp;and&nbsp;active&nbsp;by&nbsp;default,</div>
<div>&nbsp;&nbsp;we&nbsp;have&nbsp;to&nbsp;disable&nbsp;it&nbsp;to&nbsp;protect&nbsp;the&nbsp;system:</div>
<div>;&nbsp;mkdir&nbsp;-p&nbsp;/etc/polkit-1/rules.d</div>
<div>;&nbsp;echo&nbsp;'polkit.addAdminRule(function(action,&nbsp;subject)&nbsp;{&nbsp;return&nbsp;[];&nbsp;});'&nbsp;&gt;</div>
<div>&nbsp;&nbsp;/etc/polkit-1/rules.d/49-rootpw_global.rules</div>
<div></div>
<div>always&nbsp;use&nbsp;a&nbsp;different&nbsp;password&nbsp;for&nbsp;root;&nbsp;because&nbsp;"su"&nbsp;is&nbsp;always&nbsp;present&nbsp;in&nbsp;a&nbsp;Linux&nbsp;system,</div>
<div>&nbsp;&nbsp;and&nbsp;since&nbsp;it&nbsp;fails&nbsp;in&nbsp;both&nbsp;cases,&nbsp;it&nbsp;can&nbsp;be&nbsp;used&nbsp;to&nbsp;take&nbsp;over&nbsp;the&nbsp;system;</div>
<div></div>
<div>the&nbsp;solution&nbsp;is&nbsp;a&nbsp;program&nbsp;which&nbsp;opens&nbsp;a&nbsp;Wayland&nbsp;window&nbsp;with&nbsp;a&nbsp;command&nbsp;prompt;</div>
<div>if&nbsp;you&nbsp;have&nbsp;given&nbsp;your&nbsp;command&nbsp;as&nbsp;arguments&nbsp;to&nbsp;the&nbsp;program,</div>
<div>&nbsp;&nbsp;the&nbsp;prompt&nbsp;shows&nbsp;that&nbsp;command,&nbsp;as&nbsp;default&nbsp;input;</div>
<div>after&nbsp;pressing&nbsp;"return",&nbsp;it&nbsp;asks&nbsp;for&nbsp;the&nbsp;user's&nbsp;password&nbsp;(user&nbsp;must&nbsp;be&nbsp;in&nbsp;wheel&nbsp;group);</div>
<div></div>
<div>=&nbsp;automatic&nbsp;online&nbsp;atomic&nbsp;upgrades</div>
<div><a href='https://www.techrapid.uk/2017/04/automatically-update-arch-linux-with-systemd.html'>https://www.techrapid.uk/2017/04/automatically-update-arch-linux-with-systemd.html</a></div>
<div><a href='https://wiki.archlinux.org/index.php/Systemd/Timers'>https://wiki.archlinux.org/index.php/Systemd/Timers</a></div>
<div></div>
<div>to&nbsp;have&nbsp;reliable&nbsp;automatic&nbsp;updates,&nbsp;they&nbsp;must&nbsp;be&nbsp;atomic;</div>
<div></div>
<div>"usr",&nbsp;"etc"&nbsp;and&nbsp;"boot"&nbsp;must&nbsp;be&nbsp;symlinks&nbsp;to&nbsp;subvolumes;</div>
<div>create&nbsp;a&nbsp;base&nbsp;directory;</div>
<div>create&nbsp;snapshots&nbsp;of&nbsp;"usr",&nbsp;"etc"&nbsp;and&nbsp;"boot",&nbsp;then&nbsp;mount&nbsp;them&nbsp;in&nbsp;the&nbsp;base&nbsp;directory;</div>
<div>for&nbsp;the&nbsp;rest&nbsp;of&nbsp;system&nbsp;root&nbsp;directories&nbsp;make&nbsp;symlinks&nbsp;in&nbsp;the&nbsp;base&nbsp;directory;</div>
<div>chroot&nbsp;and&nbsp;upgrade;</div>
<div>remove&nbsp;the&nbsp;base&nbsp;directory;</div>
<div>change&nbsp;the&nbsp;"usr",&nbsp;"etc"&nbsp;and&nbsp;"boot"&nbsp;symlinks&nbsp;in&nbsp;the&nbsp;system&nbsp;root,</div>
<div>&nbsp;&nbsp;to&nbsp;point&nbsp;to&nbsp;the&nbsp;new&nbsp;snapshots;</div>
<h2>4, Rust</h2>
<div>Rust&nbsp;makes&nbsp;bad&nbsp;programming&nbsp;hard,&nbsp;and&nbsp;good&nbsp;programming&nbsp;fun;</div>
<div>Rust&nbsp;does&nbsp;not&nbsp;hide&nbsp;inherent&nbsp;complexity,&nbsp;in&nbsp;fact&nbsp;it&nbsp;bolds&nbsp;it,&nbsp;so&nbsp;we&nbsp;can&nbsp;see&nbsp;it,&nbsp;and&nbsp;avoid&nbsp;it;</div>
<div>by&nbsp;inherent&nbsp;complexity&nbsp;i&nbsp;mean&nbsp;a&nbsp;complexity&nbsp;which&nbsp;can&nbsp;not&nbsp;be&nbsp;abstracted&nbsp;away&nbsp;completely;</div>
<div>&nbsp;&nbsp;ie&nbsp;if&nbsp;we&nbsp;try&nbsp;to&nbsp;hide&nbsp;it,&nbsp;it&nbsp;will&nbsp;re_emerge&nbsp;somewhere&nbsp;else;</div>
<div>in&nbsp;fact,&nbsp;hiding&nbsp;inherent&nbsp;complexity&nbsp;usually&nbsp;leads&nbsp;to&nbsp;choosing&nbsp;the&nbsp;wrong&nbsp;approach;</div>
<div></div>
<div>sharing&nbsp;mutable&nbsp;data,</div>
<div>&nbsp;&nbsp;ie&nbsp;having&nbsp;a&nbsp;mutable&nbsp;reference&nbsp;to&nbsp;some&nbsp;data,&nbsp;while&nbsp;it&nbsp;is&nbsp;shared&nbsp;using&nbsp;other&nbsp;references,</div>
<div>&nbsp;&nbsp;is&nbsp;the&nbsp;root&nbsp;of&nbsp;many&nbsp;inherent&nbsp;complexities;</div>
<div><a href='https://manishearth.github.io/blog/2015/05/17/the-problem-with-shared-mutability/'>https://manishearth.github.io/blog/2015/05/17/the-problem-with-shared-mutability/</a></div>
<div>the&nbsp;basic&nbsp;problem&nbsp;in&nbsp;concurrent&nbsp;programming&nbsp;is&nbsp;sharing&nbsp;mutable&nbsp;data;</div>
<div></div>
<div>a&nbsp;data&nbsp;race&nbsp;happens&nbsp;when&nbsp;these&nbsp;three&nbsp;behaviors&nbsp;occur:</div>
<div>,&nbsp;two&nbsp;or&nbsp;more&nbsp;pointers&nbsp;access&nbsp;the&nbsp;same&nbsp;data&nbsp;at&nbsp;the&nbsp;same&nbsp;time;</div>
<div>,&nbsp;at&nbsp;least&nbsp;one&nbsp;of&nbsp;the&nbsp;pointers&nbsp;is&nbsp;being&nbsp;used&nbsp;to&nbsp;write&nbsp;to&nbsp;the&nbsp;data;</div>
<div>,&nbsp;there’s&nbsp;no&nbsp;mechanism&nbsp;being&nbsp;used&nbsp;to&nbsp;synchronize&nbsp;access&nbsp;to&nbsp;the&nbsp;data;</div>
<div></div>
<div>to&nbsp;prevent&nbsp;sharing&nbsp;mutable&nbsp;data,&nbsp;we&nbsp;can&nbsp;abandon&nbsp;mutability&nbsp;like&nbsp;in&nbsp;Haskell;</div>
<div>but&nbsp;since&nbsp;mutability&nbsp;is&nbsp;necessary&nbsp;any&nbsp;way,&nbsp;it&nbsp;introduces&nbsp;a&nbsp;complicated&nbsp;mechanism&nbsp;(Monads);</div>
<div></div>
<div>another&nbsp;approach&nbsp;to&nbsp;deal&nbsp;with&nbsp;concurrently&nbsp;shared&nbsp;mutable&nbsp;data,&nbsp;is&nbsp;the&nbsp;one&nbsp;used&nbsp;in&nbsp;Pony;</div>
<div><a href='https://www.ponylang.io/'>https://www.ponylang.io/</a></div>
<div>it&nbsp;doesn't&nbsp;abandon&nbsp;aliasing&nbsp;nor&nbsp;mutability,</div>
<div>&nbsp;&nbsp;instead&nbsp;it&nbsp;controls&nbsp;them&nbsp;using&nbsp;reference&nbsp;capabilities;</div>
<div>Pony's&nbsp;approach&nbsp;introduces&nbsp;many&nbsp;complexities,&nbsp;especially&nbsp;when&nbsp;dealing&nbsp;with&nbsp;generics;</div>
<div></div>
<div>a&nbsp;better&nbsp;approach&nbsp;is&nbsp;done&nbsp;by&nbsp;Rust,&nbsp;a&nbsp;language&nbsp;which&nbsp;overall&nbsp;has&nbsp;a&nbsp;better&nbsp;design&nbsp;too;</div>
<div>this&nbsp;approach&nbsp;even&nbsp;allows&nbsp;Rust&nbsp;to&nbsp;limit&nbsp;reference&nbsp;counting,</div>
<div>&nbsp;&nbsp;to&nbsp;situations&nbsp;where&nbsp;it's&nbsp;absolutely&nbsp;necessary;</div>
<div>Pony_like&nbsp;actors&nbsp;can&nbsp;be&nbsp;done&nbsp;in&nbsp;Rust&nbsp;using&nbsp;"may_actor";</div>
<div><a href='https://crates.io/crates/may_actor'>https://crates.io/crates/may_actor</a></div>
<div></div>
<div>,&nbsp;immutable&nbsp;data&nbsp;will&nbsp;be&nbsp;wrapped&nbsp;in&nbsp;"Arc",&nbsp;and&nbsp;we&nbsp;have&nbsp;direct&nbsp;access&nbsp;to&nbsp;it;</div>
<div>,&nbsp;mutable&nbsp;data&nbsp;will&nbsp;be&nbsp;wrapped&nbsp;in&nbsp;"Actor"&nbsp;and&nbsp;we&nbsp;can&nbsp;access&nbsp;it&nbsp;only&nbsp;through&nbsp;the&nbsp;actor&nbsp;itself;</div>
<div></div>
<div>async&nbsp;access:&nbsp;Rc</div>
<div>async&nbsp;access&nbsp;from&nbsp;multiple&nbsp;threads:&nbsp;Arc</div>
<div>async&nbsp;mutable&nbsp;access:&nbsp;Rc&lt;RefCell&gt;</div>
<div>async&nbsp;mutable&nbsp;access&nbsp;from&nbsp;mutiple&nbsp;threads:&nbsp;Actor</div>
<div></div>
<div>it's&nbsp;better&nbsp;to&nbsp;use&nbsp;"&mut"&nbsp;only&nbsp;for&nbsp;the&nbsp;receiver&nbsp;in&nbsp;methods;</div>
<div>and&nbsp;anywhere&nbsp;else&nbsp;use&nbsp;actors&nbsp;to&nbsp;control&nbsp;mutability;</div>
<div></div>
<div>static&nbsp;data&nbsp;(functions,&nbsp;structs,&nbsp;constants):&nbsp;no&nbsp;problem,&nbsp;copy&nbsp;or&nbsp;share&nbsp;by&nbsp;reference;</div>
<div>dynamic&nbsp;data:</div>
<div>,&nbsp;if&nbsp;data&nbsp;is&nbsp;small&nbsp;we&nbsp;usually&nbsp;put&nbsp;it&nbsp;on&nbsp;stack;</div>
<div>&nbsp;&nbsp;so&nbsp;we&nbsp;don't&nbsp;share&nbsp;it&nbsp;across&nbsp;the&nbsp;program,&nbsp;we&nbsp;copy&nbsp;it;</div>
<div>,&nbsp;if&nbsp;data&nbsp;is&nbsp;big&nbsp;we&nbsp;put&nbsp;it&nbsp;on&nbsp;heap&nbsp;and&nbsp;make&nbsp;references&nbsp;to&nbsp;it;</div>
<div>&nbsp;&nbsp;if&nbsp;data&nbsp;is&nbsp;immutable&nbsp;we&nbsp;just&nbsp;have&nbsp;to&nbsp;manage&nbsp;its&nbsp;lifetime,</div>
<div>&nbsp;&nbsp;&nbsp;&nbsp;either&nbsp;statically&nbsp;(using&nbsp;"&"),&nbsp;or&nbsp;dynamically&nbsp;(using&nbsp;"Arc")</div>
<div>&nbsp;&nbsp;but&nbsp;if&nbsp;data&nbsp;is&nbsp;mutable&nbsp;we&nbsp;have&nbsp;to&nbsp;check&nbsp;if&nbsp;the&nbsp;read_write_lock&nbsp;pattern&nbsp;is&nbsp;fulfilled,</div>
<div>&nbsp;&nbsp;&nbsp;&nbsp;using&nbsp;"&mut"&nbsp;or&nbsp;"Actor";</div>
<div>&nbsp;&nbsp;"Mutex"&nbsp;or&nbsp;"RwLock"&nbsp;check&nbsp;read_write_lock&nbsp;pattern&nbsp;at&nbsp;runtime,</div>
<div>&nbsp;&nbsp;&nbsp;&nbsp;and&nbsp;make&nbsp;the&nbsp;program&nbsp;to&nbsp;panic,&nbsp;if&nbsp;it's&nbsp;failed;</div>
<div></div>
<div>in&nbsp;Rust&nbsp;any&nbsp;resource&nbsp;have&nbsp;exactly&nbsp;one&nbsp;owner&nbsp;which&nbsp;takes&nbsp;care&nbsp;of&nbsp;its&nbsp;resource&nbsp;deallocation;</div>
<div>owners&nbsp;can&nbsp;share&nbsp;their&nbsp;data&nbsp;by&nbsp;lending&nbsp;them&nbsp;to&nbsp;references;</div>
<div>references&nbsp;must&nbsp;have&nbsp;a&nbsp;lifetime&nbsp;less&nbsp;than&nbsp;the&nbsp;owner;</div>
<div>furthermore&nbsp;lifetime&nbsp;of&nbsp;a&nbsp;mutable&nbsp;reference&nbsp;must&nbsp;not&nbsp;overlap&nbsp;with&nbsp;other&nbsp;references;</div>
<div><a href='http://blog.skylight.io/rust-means-never-having-to-close-a-socket/'>http://blog.skylight.io/rust-means-never-having-to-close-a-socket/</a></div>
<div></div>
<div>owner&nbsp;can:</div>
<div>,&nbsp;access&nbsp;and&nbsp;mutate&nbsp;the&nbsp;resource;</div>
<div>,&nbsp;lend&nbsp;the&nbsp;resource&nbsp;to&nbsp;a&nbsp;reference;</div>
<div>,&nbsp;hand&nbsp;over&nbsp;ownership&nbsp;(move),&nbsp;or&nbsp;deallocate&nbsp;resource;</div>
<div>but&nbsp;during&nbsp;a&nbsp;lend,&nbsp;owner&nbsp;can't:</div>
<div>,&nbsp;mutate&nbsp;the&nbsp;resource;</div>
<div>,&nbsp;mutably&nbsp;lend&nbsp;resource&nbsp;to&nbsp;another&nbsp;reference;</div>
<div>,&nbsp;hand&nbsp;over&nbsp;ownership&nbsp;(move),&nbsp;or&nbsp;deallocate&nbsp;resource;</div>
<div>and&nbsp;during&nbsp;a&nbsp;mutable&nbsp;lend,&nbsp;owner&nbsp;can't&nbsp;even&nbsp;access&nbsp;the&nbsp;resource;</div>
<div></div>
<div>immutable&nbsp;(and&nbsp;thus&nbsp;sharable)&nbsp;references&nbsp;can:</div>
<div>,&nbsp;access&nbsp;borrowed&nbsp;resource;</div>
<div>,&nbsp;immutably&nbsp;lend&nbsp;resource&nbsp;to&nbsp;other&nbsp;references;</div>
<div>mutable&nbsp;(and&nbsp;thus&nbsp;exclusive)&nbsp;reference&nbsp;can:</div>
<div>,&nbsp;access&nbsp;and&nbsp;mutate&nbsp;resource;</div>
<div>,&nbsp;mutably&nbsp;lend&nbsp;resource&nbsp;to&nbsp;another&nbsp;reference;</div>
<div>,&nbsp;immutably&nbsp;lend&nbsp;resource,&nbsp;but&nbsp;during&nbsp;this&nbsp;lending,&nbsp;they&nbsp;can't&nbsp;mutate&nbsp;it;</div>
<div>&nbsp;&nbsp;just&nbsp;like&nbsp;when&nbsp;an&nbsp;owner&nbsp;immutably&nbsp;lends&nbsp;its&nbsp;resource;</div>
<div></div>
<div>during&nbsp;shared&nbsp;borrow&nbsp;(immutable&nbsp;borrow)&nbsp;no&nbsp;one&nbsp;owns&nbsp;the&nbsp;data;</div>
<div>&nbsp;&nbsp;so&nbsp;even&nbsp;the&nbsp;original&nbsp;owner&nbsp;can't&nbsp;change&nbsp;it;</div>
<div>during&nbsp;mutable&nbsp;borrow&nbsp;the&nbsp;(unique)&nbsp;borrower&nbsp;owns&nbsp;it;</div>
<div>so&nbsp;"&mut"&nbsp;is&nbsp;actually&nbsp;a&nbsp;temporary&nbsp;transfer&nbsp;of&nbsp;ownership;</div>
<div></div>
<div>s:&nbsp;String&nbsp;-&gt;&nbsp;&s:&nbsp;&String&nbsp;-&gt;&nbsp;&s[..]:&nbsp;&str</div>
<div>v:&nbsp;Vec&lt;T&gt;&nbsp;-&gt;&nbsp;&v:&nbsp;&Vec&lt;T&gt;&nbsp;-&gt;&nbsp;&v[..]:&nbsp;&[T]</div>
<div>&str&nbsp;and&nbsp;&[T]&nbsp;are&nbsp;slices;&nbsp;str&nbsp;and&nbsp;[T]&nbsp;are&nbsp;unsized&nbsp;types;</div>
<div>slicing&nbsp;is&nbsp;like&nbsp;borrowing&nbsp;from&nbsp;an&nbsp;unsized&nbsp;type;</div>
<div>since&nbsp;the&nbsp;slice&nbsp;contains&nbsp;the&nbsp;size,&nbsp;the&nbsp;lending&nbsp;type&nbsp;itself&nbsp;doesn't&nbsp;need&nbsp;to&nbsp;have&nbsp;a&nbsp;definite&nbsp;size;</div>
<div></div>
<div>x&nbsp;=&nbsp;a[i]&nbsp;-&gt;&nbsp;this&nbsp;is&nbsp;possible&nbsp;if&nbsp;the&nbsp;elements&nbsp;of&nbsp;"a"&nbsp;are&nbsp;copy</div>
<div>&nbsp;&nbsp;(cause&nbsp;moving&nbsp;out&nbsp;of&nbsp;collections&nbsp;is&nbsp;not&nbsp;possible);</div>
<div>x&nbsp;=&nbsp;&a[i]&nbsp;-&gt;&nbsp;this&nbsp;is&nbsp;for&nbsp;the&nbsp;case&nbsp;when&nbsp;the&nbsp;elements&nbsp;are&nbsp;not&nbsp;copy;</div>
<div>x&nbsp;=&nbsp;a[i..j]&nbsp;-&gt;&nbsp;this&nbsp;is&nbsp;always&nbsp;invalid;</div>
<div>x&nbsp;=&nbsp;&a[i..j]&nbsp;-&gt;&nbsp;slicing;</div>
<div></div>
<div>auto&nbsp;ref/deref&nbsp;for&nbsp;self&nbsp;in&nbsp;method&nbsp;calls:</div>
<div>&nbsp;&nbsp;compiler&nbsp;inserts&nbsp;as&nbsp;many&nbsp;*&nbsp;or&nbsp;&&nbsp;as&nbsp;necessary&nbsp;to&nbsp;get&nbsp;it&nbsp;right;</div>
<div>because&nbsp;in&nbsp;method&nbsp;calls&nbsp;name&nbsp;and&nbsp;context&nbsp;of&nbsp;a&nbsp;method&nbsp;call&nbsp;is&nbsp;almost&nbsp;always&nbsp;sufficient</div>
<div>&nbsp;&nbsp;to&nbsp;infer&nbsp;the&nbsp;move/borrow&nbsp;semantics;</div>
<div></div>
<div>deref&nbsp;coercion:</div>
<div>,&nbsp;&T&nbsp;-&gt;&nbsp;&U&nbsp;when&nbsp;T:&nbsp;Deref&lt;Target=U&gt;</div>
<div>,&nbsp;&mut&nbsp;T&nbsp;-&gt;&nbsp;&U&nbsp;when&nbsp;T:&nbsp;Deref&lt;Target=U&gt;</div>
<div>,&nbsp;&mut&nbsp;T&nbsp;-&gt;&nbsp;&mut&nbsp;U&nbsp;when&nbsp;T:&nbsp;DerefMut&lt;Target=U&gt;</div>
<div>examples:</div>
<div>&nbsp;&nbsp;&&i32&nbsp;-&gt;&nbsp;&i32&nbsp;because&nbsp;&i32:&nbsp;Deref&lt;Target=i32&gt;</div>
<div>&nbsp;&nbsp;&String&nbsp;-&gt;&nbsp;&str&nbsp;because&nbsp;String:&nbsp;Deref&lt;Target=str&gt;</div>
<div>&nbsp;&nbsp;&Vec&lt;T&gt;&nbsp;-&gt;&nbsp;&[T]&nbsp;because&nbsp;Vec&lt;T&gt;:&nbsp;Deref&lt;Target=[T]&gt;</div>
<div>&nbsp;&nbsp;&Arc&lt;T&gt;&nbsp;-&gt;&nbsp;&T&nbsp;because&nbsp;Arc&lt;T&gt;:&nbsp;Deref&lt;Target=T&gt;</div>
<div><a href='https://github.com/rust-lang/rfcs/blob/master/text/0241-deref-conversions.md'>https://github.com/rust-lang/rfcs/blob/master/text/0241-deref-conversions.md</a></div>
<div></div>
<div>=&nbsp;type&nbsp;system</div>
<div>types&nbsp;show&nbsp;us&nbsp;what&nbsp;we&nbsp;can&nbsp;do&nbsp;with&nbsp;the&nbsp;data,&nbsp;ie&nbsp;which&nbsp;operations&nbsp;are&nbsp;valid;</div>
<div></div>
<div>the&nbsp;class&nbsp;hierarchy&nbsp;design,&nbsp;like&nbsp;the&nbsp;one&nbsp;in&nbsp;Java,&nbsp;is&nbsp;problematic;</div>
<div>&nbsp;&nbsp;<a href='http://ptgmedia.pearsoncmg.com/images/020163371x/items/item33.html'>http://ptgmedia.pearsoncmg.com/images/020163371x/items/item33.html</a></div>
<div>also&nbsp;the&nbsp;problem&nbsp;of&nbsp;covariance&nbsp;for&nbsp;generic&nbsp;types,&nbsp;has&nbsp;its&nbsp;root&nbsp;in&nbsp;this&nbsp;problem;</div>
<div>&nbsp;&nbsp;<a href='https://en.wikipedia.org/wiki/Wildcard_(Java)'>https://en.wikipedia.org/wiki/Wildcard_(Java)</a></div>
<div>i&nbsp;think&nbsp;this&nbsp;problem&nbsp;is&nbsp;also&nbsp;the&nbsp;motivation&nbsp;for&nbsp;dynamic&nbsp;typing&nbsp;(another&nbsp;bad&nbsp;design);</div>
<div>the&nbsp;right&nbsp;way&nbsp;as&nbsp;done&nbsp;in&nbsp;Pony&nbsp;and&nbsp;Rust:</div>
<div>,&nbsp;concrete&nbsp;types&nbsp;(like&nbsp;final&nbsp;classes&nbsp;in&nbsp;Java)&nbsp;can&nbsp;be&nbsp;instantiated,&nbsp;but&nbsp;cannot&nbsp;have&nbsp;subtypes;</div>
<div>,&nbsp;abstract&nbsp;types&nbsp;(like&nbsp;abstract&nbsp;classes&nbsp;in&nbsp;Java)&nbsp;cannot&nbsp;be&nbsp;instantiated,&nbsp;but&nbsp;can&nbsp;have&nbsp;subtypes;</div>
<div></div>
<div>note&nbsp;that&nbsp;"x.m()"&nbsp;is&nbsp;method&nbsp;call&nbsp;syntax,&nbsp;which&nbsp;completely&nbsp;differs&nbsp;from&nbsp;"(x.m)()";</div>
<div></div>
<div>an&nbsp;absolute&nbsp;path&nbsp;starts&nbsp;from&nbsp;a&nbsp;crate&nbsp;root&nbsp;by&nbsp;using&nbsp;a&nbsp;crate&nbsp;name&nbsp;or&nbsp;a&nbsp;literal&nbsp;"crate";</div>
<div>a&nbsp;relative&nbsp;path&nbsp;starts&nbsp;from&nbsp;the&nbsp;current&nbsp;module&nbsp;and&nbsp;uses</div>
<div>&nbsp;&nbsp;"self",&nbsp;"super",&nbsp;or&nbsp;the&nbsp;name&nbsp;of&nbsp;an&nbsp;item&nbsp;in&nbsp;the&nbsp;current&nbsp;module;</div>
<div>if&nbsp;an&nbsp;in_scope&nbsp;item&nbsp;has&nbsp;the&nbsp;same&nbsp;name&nbsp;as&nbsp;a&nbsp;crate,&nbsp;then&nbsp;we&nbsp;have&nbsp;to&nbsp;resolve&nbsp;the&nbsp;ambiguity:</div>
<div>,&nbsp;using&nbsp;a&nbsp;leading&nbsp;"::"&nbsp;for&nbsp;a&nbsp;crate&nbsp;name;</div>
<div>,&nbsp;using&nbsp;a&nbsp;leading&nbsp;"self::"&nbsp;for&nbsp;an&nbsp;in_scope&nbsp;item;</div>
<div></div>
<div>arrays&nbsp;like&nbsp;tuples&nbsp;have&nbsp;fixed&nbsp;size&nbsp;and&nbsp;thus&nbsp;stored&nbsp;on&nbsp;stack;</div>
<div>but&nbsp;since&nbsp;they&nbsp;are&nbsp;homogeneous&nbsp;(all&nbsp;elements&nbsp;are&nbsp;of&nbsp;the&nbsp;same&nbsp;type),</div>
<div>&nbsp;&nbsp;they&nbsp;can&nbsp;be&nbsp;indexed&nbsp;at&nbsp;runtime;</div>
<div>vectors&nbsp;and&nbsp;hash&nbsp;tables&nbsp;are&nbsp;homogeneous,&nbsp;varying&nbsp;sized&nbsp;collections;</div>
<div></div>
<div>Rust&nbsp;does&nbsp;not&nbsp;have&nbsp;named&nbsp;arguments&nbsp;and&nbsp;named&nbsp;tuples;&nbsp;and&nbsp;it's&nbsp;a&nbsp;good&nbsp;thing;</div>
<div>when&nbsp;you&nbsp;need&nbsp;functions&nbsp;with&nbsp;lots&nbsp;of&nbsp;arguments,&nbsp;or&nbsp;tuples&nbsp;with&nbsp;lots&nbsp;of&nbsp;elements,</div>
<div>&nbsp;&nbsp;it's&nbsp;a&nbsp;sign&nbsp;that&nbsp;you&nbsp;need&nbsp;to&nbsp;restructure&nbsp;your&nbsp;code,&nbsp;and&nbsp;use&nbsp;structs&nbsp;to&nbsp;define&nbsp;new&nbsp;types;</div>
<div></div>
<div>a&nbsp;closure&nbsp;is&nbsp;like&nbsp;an&nbsp;anonymous&nbsp;struct&nbsp;made&nbsp;of&nbsp;variables&nbsp;captured&nbsp;from&nbsp;environment,</div>
<div>&nbsp;&nbsp;that&nbsp;is&nbsp;callable&nbsp;(implements&nbsp;Fn/FnMut/FnOnce&nbsp;trait);</div>
<div>so&nbsp;all&nbsp;closures&nbsp;are&nbsp;unique&nbsp;types,&nbsp;but&nbsp;they&nbsp;have&nbsp;traits&nbsp;in&nbsp;common;</div>
<div>note&nbsp;that&nbsp;if&nbsp;we&nbsp;put&nbsp;a&nbsp;generic&nbsp;type&nbsp;parameter&nbsp;in&nbsp;the&nbsp;return&nbsp;type&nbsp;of&nbsp;a&nbsp;function,</div>
<div>&nbsp;&nbsp;we&nbsp;have&nbsp;to&nbsp;provide&nbsp;the&nbsp;concrete&nbsp;type&nbsp;when&nbsp;we&nbsp;call&nbsp;the&nbsp;function;</div>
<div>&nbsp;&nbsp;thus&nbsp;we&nbsp;can't&nbsp;use&nbsp;generic&nbsp;type&nbsp;parameters&nbsp;to&nbsp;return&nbsp;a&nbsp;closure,&nbsp;we&nbsp;have&nbsp;to&nbsp;use&nbsp;"impl";</div>
<div></div>
<div>"fn(T1)&nbsp;-&gt;&nbsp;T2"&nbsp;is&nbsp;not&nbsp;an&nbsp;unsized&nbsp;type&nbsp;like&nbsp;"str",&nbsp;it's&nbsp;a&nbsp;function&nbsp;pointer;</div>
<div></div>
<div><a href='https://crates.io/crates/serde'>https://crates.io/crates/serde</a></div>
<div><a href='https://github.com/ron-rs/ron'>https://github.com/ron-rs/ron</a></div>
<div></div>
<div>math:</div>
<div><a href='https://nalgebra.org/rustdoc/nalgebra/index.html'>https://nalgebra.org/rustdoc/nalgebra/index.html</a></div>
<div><a href='https://gitlab.com/ornamentist/un-algebra'>https://gitlab.com/ornamentist/un-algebra</a></div>
<div><a href='https://github.com/rustsim/alga'>https://github.com/rustsim/alga</a></div>
<div></div>
<div>machine&nbsp;learning:</div>
<div>methods&nbsp;that&nbsp;their&nbsp;operation&nbsp;depends&nbsp;on&nbsp;adjustable&nbsp;fields;</div>
<div></div>
<div>install&nbsp;"rustup"&nbsp;and&nbsp;"gcc"&nbsp;then:</div>
<div>;&nbsp;rustup&nbsp;default&nbsp;stable</div>
<div>to&nbsp;update&nbsp;Rust:</div>
<div>;&nbsp;rustup&nbsp;update</div>
<h2>5, Git</h2>
<div>the&nbsp;problem&nbsp;of&nbsp;shared&nbsp;mutable&nbsp;data&nbsp;is&nbsp;so&nbsp;pervasive&nbsp;in&nbsp;the&nbsp;computing&nbsp;world;</div>
<div>we&nbsp;can&nbsp;also&nbsp;see&nbsp;it&nbsp;in&nbsp;file&nbsp;synchronization&nbsp;tools&nbsp;like&nbsp;Git:</div>
<div>,&nbsp;there&nbsp;can&nbsp;be&nbsp;conflicts&nbsp;when&nbsp;pushing&nbsp;or&nbsp;pulling,&nbsp;that&nbsp;must&nbsp;be&nbsp;resolved&nbsp;manually;</div>
<div>,&nbsp;the&nbsp;history&nbsp;just&nbsp;grows&nbsp;indefinitely&nbsp;cause&nbsp;change&nbsp;in&nbsp;the&nbsp;history&nbsp;of&nbsp;the&nbsp;repository&nbsp;can&nbsp;be&nbsp;catastrophic;</div>
<div></div>
<div>solution:</div>
<div>only&nbsp;the&nbsp;owner&nbsp;can&nbsp;mutate&nbsp;the&nbsp;repository;</div>
<div>owner&nbsp;can&nbsp;mutably&nbsp;borrow&nbsp;the&nbsp;repository&nbsp;for&nbsp;a&nbsp;defined&nbsp;duration,&nbsp;or&nbsp;move&nbsp;ownership;</div>
<div>others&nbsp;have&nbsp;to&nbsp;send&nbsp;messages:</div>
<div>,&nbsp;in&nbsp;the&nbsp;form&nbsp;of&nbsp;to_dos&nbsp;define&nbsp;by&nbsp;the&nbsp;owner;</div>
<div>,&nbsp;corrections</div>
<div></div>
<div>immutable&nbsp;repositories&nbsp;can&nbsp;apply&nbsp;corrections&nbsp;and&nbsp;to_dos,&nbsp;and&nbsp;test&nbsp;them,</div>
<div>&nbsp;&nbsp;but&nbsp;they&nbsp;can't&nbsp;push&nbsp;it&nbsp;to&nbsp;remote,&nbsp;only&nbsp;the&nbsp;owner&nbsp;can;</div>
<div>automatic&nbsp;sync&nbsp;at&nbsp;the&nbsp;immutable&nbsp;ends;</div>
<div>sync&nbsp;before&nbsp;sending&nbsp;to_dos&nbsp;or&nbsp;corrections;</div>
<div></div>
<div><a href='https://git-scm.com/docs/git-config#Documentation/git-config.txt-httpcookieFile'>https://git-scm.com/docs/git-config#Documentation/git-config.txt-httpcookieFile</a></div>
<div>gitless</div>
<div><a href='https://people.gnome.org/~newren/eg/'>https://people.gnome.org/~newren/eg/</a></div>
<div></div>
<div>delete&nbsp;old&nbsp;and&nbsp;big&nbsp;history;</div>
<div>for&nbsp;text&nbsp;files&nbsp;at&nbsp;least&nbsp;keep&nbsp;the&nbsp;last&nbsp;version&nbsp;(for&nbsp;diffing);</div>
<div>for&nbsp;binaries&nbsp;delete&nbsp;all;</div>
<div>clone&nbsp;only&nbsp;the&nbsp;last&nbsp;version;</div>
<div>git&nbsp;shallow&nbsp;clones</div>
<div>git&nbsp;partial&nbsp;clones</div>
<div></div>
<div>Gitlab:</div>
<div><a href='https://gist.github.com/gpocentek/bd4c3fbf8a6ce226ebddc4aad6b46c0a'>https://gist.github.com/gpocentek/bd4c3fbf8a6ce226ebddc4aad6b46c0a</a></div>
<div>".gitignore"&nbsp;and&nbsp;".gitlab-ci.yml"</div>
<div>create&nbsp;readme&nbsp;with&nbsp;CI;</div>
<div><a href='https://stackoverflow.com/questions/38807677/use-gitlab-api-from-a-gitlabci-build-script"'>https://stackoverflow.com/questions/38807677/use-gitlab-api-from-a-gitlabci-build-script"</a></div>
<div><a href='https://www.reddit.com/r/devops/comments/adhg4x/how_are_gitlab_ci_environment_variables_are/'>https://www.reddit.com/r/devops/comments/adhg4x/how_are_gitlab_ci_environment_variables_are/</a></div>
<div><a href='https://gitlab.com/gitlab-org/gitlab/-/issues/20416'>https://gitlab.com/gitlab-org/gitlab/-/issues/20416</a></div>
<div><a href='https://docs.gitlab.com/ee/ci/quick_start/'>https://docs.gitlab.com/ee/ci/quick_start/</a></div>
<div><a href='https://docs.gitlab.com/ee/user/project/push_options.html#push-options-for-gitlab-cicd'>https://docs.gitlab.com/ee/user/project/push_options.html#push-options-for-gitlab-cicd</a></div>
<div><a href='https://docs.gitlab.com/ee/ci/variables/README.html'>https://docs.gitlab.com/ee/ci/variables/README.html</a></div>
<div>personal&nbsp;access&nbsp;token&nbsp;put&nbsp;in&nbsp;a&nbsp;masked&nbsp;variable;</div>
<div><a href='https://fromthebottomoftheheap.net/2020/04/30/rendering-your-readme-with-github-actions/'>https://fromthebottomoftheheap.net/2020/04/30/rendering-your-readme-with-github-actions/</a></div>
<div><a href='https://github.com/marketplace/actions/generate-update-markdown-content'>https://github.com/marketplace/actions/generate-update-markdown-content</a></div>
<div><a href='https://github.com/theboi/github-update-readme'>https://github.com/theboi/github-update-readme</a></div>
<div>push_to_create&nbsp;creates&nbsp;private&nbsp;repositories,&nbsp;and&nbsp;there&nbsp;is&nbsp;no&nbsp;push_options&nbsp;to&nbsp;make&nbsp;them&nbsp;public,&nbsp;yet;</div>
<div>also&nbsp;we&nbsp;can't&nbsp;remove&nbsp;repositories;</div>
<div>so&nbsp;we&nbsp;must&nbsp;use&nbsp;push&nbsp;options&nbsp;for&nbsp;gitlab&nbsp;ci/cd;</div>
<div></div>
<div><a href='https://about.gitlab.com/blog/2016/12/07/building-a-new-gitlab-docs-site-with-nanoc-gitlab-ci-and-gitlab-pages/'>https://about.gitlab.com/blog/2016/12/07/building-a-new-gitlab-docs-site-with-nanoc-gitlab-ci-and-gitlab-pages/</a></div>
<h2>6, graphics</h2>
<div>software&nbsp;rendering&nbsp;gives&nbsp;us&nbsp;a&nbsp;lot&nbsp;more&nbsp;flexibility,</div>
<div>&nbsp;&nbsp;because&nbsp;we&nbsp;won't&nbsp;be&nbsp;limited&nbsp;by&nbsp;a&nbsp;hardware&nbsp;implementation,</div>
<div>&nbsp;&nbsp;with&nbsp;triangle&nbsp;only&nbsp;rasterization,&nbsp;isolated&nbsp;shader&nbsp;programs,&nbsp;and&nbsp;fixed&nbsp;size&nbsp;buffers;</div>
<div>GPU&nbsp;equivalent&nbsp;performance&nbsp;can&nbsp;be&nbsp;achieved&nbsp;using&nbsp;SIMD;</div>
<div></div>
<div>graphical&nbsp;objects&nbsp;are&nbsp;made&nbsp;of&nbsp;primitives;</div>
<div>&nbsp;&nbsp;each&nbsp;primitive&nbsp;has&nbsp;a&nbsp;specific&nbsp;algorithm&nbsp;for&nbsp;rasterization;</div>
<div>2d&nbsp;primitives:&nbsp;point,&nbsp;line,&nbsp;curve,&nbsp;triangle,&nbsp;curved&nbsp;area;</div>
<div>3d&nbsp;objects&nbsp;made&nbsp;of&nbsp;flat&nbsp;surfaces&nbsp;will&nbsp;be&nbsp;broken&nbsp;up&nbsp;into&nbsp;triangles;</div>
<div>3d&nbsp;objects&nbsp;made&nbsp;of&nbsp;curved&nbsp;surfaces&nbsp;can&nbsp;be&nbsp;broken&nbsp;up&nbsp;into&nbsp;a&nbsp;number&nbsp;of&nbsp;primitive&nbsp;3d&nbsp;surfaces,</div>
<div>&nbsp;&nbsp;which&nbsp;can&nbsp;be&nbsp;easily&nbsp;projected&nbsp;to&nbsp;2d;</div>
<div><a href='https://en.wikipedia.org/wiki/Quadric'>https://en.wikipedia.org/wiki/Quadric</a></div>
<div>also&nbsp;interpolation&nbsp;is&nbsp;a&nbsp;good&nbsp;method&nbsp;for&nbsp;amorphous&nbsp;surfaces;</div>
<div></div>
<div>2d&nbsp;objects&nbsp;will&nbsp;be&nbsp;rasterized&nbsp;into&nbsp;pixels&nbsp;(a&nbsp;pixel&nbsp;is&nbsp;a&nbsp;coordinate&nbsp;plus&nbsp;a&nbsp;color&nbsp;value);</div>
<div>then&nbsp;these&nbsp;rasterized&nbsp;objects&nbsp;will&nbsp;be&nbsp;drawn&nbsp;in&nbsp;the&nbsp;framebuffer,</div>
<div>&nbsp;&nbsp;in&nbsp;layers&nbsp;over&nbsp;each&nbsp;other&nbsp;(in&nbsp;an&nbsp;overlay&nbsp;on&nbsp;top&nbsp;of&nbsp;all&nbsp;3d&nbsp;objects);</div>
<div>rasterizing&nbsp;3d&nbsp;objects,&nbsp;produces&nbsp;an&nbsp;array&nbsp;of&nbsp;fragments;</div>
<div>&nbsp;&nbsp;a&nbsp;fragment,&nbsp;besides&nbsp;color,&nbsp;contains&nbsp;a&nbsp;normal&nbsp;and&nbsp;a&nbsp;depth;</div>
<div>when&nbsp;creating&nbsp;the&nbsp;pixels&nbsp;of&nbsp;the&nbsp;framebuffer&nbsp;from&nbsp;the&nbsp;fragments,</div>
<div>&nbsp;&nbsp;the&nbsp;normals&nbsp;are&nbsp;used&nbsp;for&nbsp;lighting,&nbsp;and&nbsp;the&nbsp;depths&nbsp;are&nbsp;used&nbsp;for&nbsp;z_buffer;</div>
<div></div>
<div>graphical&nbsp;objects&nbsp;are&nbsp;of&nbsp;2&nbsp;kinds:</div>
<div>,&nbsp;those&nbsp;which&nbsp;we&nbsp;know&nbsp;will&nbsp;remain&nbsp;unchanged&nbsp;the&nbsp;next&nbsp;time&nbsp;we&nbsp;want&nbsp;to&nbsp;draw&nbsp;to&nbsp;the&nbsp;framebuffer;</div>
<div>&nbsp;&nbsp;these&nbsp;objects&nbsp;are&nbsp;first&nbsp;rasterized&nbsp;into&nbsp;memory,&nbsp;then&nbsp;we&nbsp;copy&nbsp;it&nbsp;into&nbsp;framebuffer;</div>
<div>,&nbsp;those&nbsp;which&nbsp;we&nbsp;know&nbsp;will&nbsp;be&nbsp;changed&nbsp;(scaled,&nbsp;rotated,&nbsp;moved&nbsp;in&nbsp;z&nbsp;direction)</div>
<div>&nbsp;&nbsp;the&nbsp;next&nbsp;time&nbsp;we&nbsp;want&nbsp;to&nbsp;draw&nbsp;to&nbsp;the&nbsp;framebuffer</div>
<div>&nbsp;&nbsp;&nbsp;&nbsp;(which&nbsp;happens&nbsp;a&nbsp;lot&nbsp;for&nbsp;animations&nbsp;with&nbsp;high&nbsp;frame&nbsp;rate);</div>
<div>&nbsp;&nbsp;these&nbsp;objects&nbsp;will&nbsp;be&nbsp;drawn&nbsp;directly&nbsp;to&nbsp;the&nbsp;framebuffer;</div>
<div>(framebuffer&nbsp;uses&nbsp;double&nbsp;buffering&nbsp;and&nbsp;v_sync;)</div>
<div>note&nbsp;that&nbsp;if&nbsp;an&nbsp;object&nbsp;just&nbsp;moves&nbsp;in&nbsp;x_y&nbsp;plane&nbsp;(without&nbsp;rotation),</div>
<div>&nbsp;&nbsp;the&nbsp;cached&nbsp;rasterization&nbsp;is&nbsp;still&nbsp;useful;</div>
<div>&nbsp;&nbsp;for&nbsp;2d&nbsp;objects&nbsp;we&nbsp;simply&nbsp;add&nbsp;a&nbsp;constant&nbsp;to&nbsp;the&nbsp;position&nbsp;of&nbsp;all&nbsp;pixels;</div>
<div>&nbsp;&nbsp;for&nbsp;3d&nbsp;objects&nbsp;we&nbsp;may&nbsp;additionally&nbsp;want&nbsp;to&nbsp;recompute&nbsp;the&nbsp;lighting&nbsp;of&nbsp;pixels&nbsp;from&nbsp;fragments;</div>
<div></div>
<div>data&nbsp;structure&nbsp;for&nbsp;graphical&nbsp;objects:</div>
<div>,&nbsp;primitives</div>
<div>,&nbsp;material</div>
<div>,&nbsp;cached&nbsp;rasterization&nbsp;(can&nbsp;be&nbsp;none)</div>
<div></div>
<div>with&nbsp;a&nbsp;scene&nbsp;graph&nbsp;we&nbsp;can&nbsp;have&nbsp;fine&nbsp;grained&nbsp;graphical&nbsp;objects&nbsp;which&nbsp;can&nbsp;be&nbsp;combined&nbsp;easily;</div>
<div></div>
<div><a href='https://en.wikipedia.org/wiki/Midpoint_circle_algorithm'>https://en.wikipedia.org/wiki/Midpoint_circle_algorithm</a></div>
<div><a href='https://en.wikipedia.org/wiki/Xiaolin_Wu%27s_line_algorithm'>https://en.wikipedia.org/wiki/Xiaolin_Wu%27s_line_algorithm</a></div>
<div><a href='http://members.chello.at/~easyfilter/bresenham.html'>http://members.chello.at/~easyfilter/bresenham.html</a></div>
<div><a href='https://nothings.org/gamedev/rasterize/'>https://nothings.org/gamedev/rasterize/</a></div>
<div><a href='https://magcius.github.io/xplain/article/'>https://magcius.github.io/xplain/article/</a></div>
<div><a href='https://en.wikipedia.org/wiki/Stencil_buffer'>https://en.wikipedia.org/wiki/Stencil_buffer</a></div>
<div><a href='https://www.scratchapixel.com'>https://www.scratchapixel.com</a></div>
<div><a href='https://www.scratchapixel.com/lessons/3d-basic-rendering/phong-shader-BRDF'>https://www.scratchapixel.com/lessons/3d-basic-rendering/phong-shader-BRDF</a></div>
<div></div>
<div><a href='https://github.com/rust-windowing/winit'>https://github.com/rust-windowing/winit</a></div>
<div><a href='https://github.com/kas-gui/kas'>https://github.com/kas-gui/kas</a></div>
<div><a href='https://github.com/sebcrozet/kiss3d'>https://github.com/sebcrozet/kiss3d</a></div>
<div><a href='https://github.com/three-rs/three'>https://github.com/three-rs/three</a></div>
<div><a href='https://crates.io/crates/rust-3d'>https://crates.io/crates/rust-3d</a></div>
<div><a href='https://github.com/38/plotters'>https://github.com/38/plotters</a></div>
<div><a href='https://github.com/rustsim/nphysics'>https://github.com/rustsim/nphysics</a></div>
<div></div>
<div>mono_space&nbsp;fonts:</div>
<div>&nbsp;&nbsp;wide&nbsp;characters&nbsp;are&nbsp;forced&nbsp;to&nbsp;squeeze;</div>
<div>&nbsp;&nbsp;narrow&nbsp;characters&nbsp;are&nbsp;forced&nbsp;to&nbsp;stretch;</div>
<div>&nbsp;&nbsp;uppercase&nbsp;letters&nbsp;look&nbsp;skinny&nbsp;next&nbsp;to&nbsp;lowercase;</div>
<div>&nbsp;&nbsp;bold&nbsp;characters&nbsp;don’t&nbsp;have&nbsp;enough&nbsp;room;</div>
<div>proportional&nbsp;font&nbsp;for&nbsp;code:</div>
<div>&nbsp;&nbsp;generous&nbsp;spacing</div>
<div>&nbsp;&nbsp;large&nbsp;punctuation</div>
<div>&nbsp;&nbsp;and&nbsp;easily&nbsp;distinguishable&nbsp;characters</div>
<div>&nbsp;&nbsp;while&nbsp;allowing&nbsp;each&nbsp;character&nbsp;to&nbsp;take&nbsp;up&nbsp;the&nbsp;space&nbsp;that&nbsp;it&nbsp;needs</div>
<div><a href='http://input.fontbureau.com/info/'>http://input.fontbureau.com/info/</a></div>
<div>for&nbsp;proportional&nbsp;fonts,&nbsp;we&nbsp;can't&nbsp;use&nbsp;spaces&nbsp;for&nbsp;text&nbsp;alignment;</div>
<div>elastic&nbsp;tabstops&nbsp;may&nbsp;help:&nbsp;<a href='http://nickgravgaard.com/elastic-tabstops/'>http://nickgravgaard.com/elastic-tabstops/</a>;</div>
<div>but&nbsp;i&nbsp;think,&nbsp;text&nbsp;alignment&nbsp;is&nbsp;a&nbsp;bad&nbsp;idea,&nbsp;in&nbsp;general;</div>
<h2>7, GUI</h2>
<div>implementing&nbsp;a&nbsp;complete&nbsp;GUI&nbsp;toolkit&nbsp;is&nbsp;a&nbsp;lot&nbsp;of&nbsp;work;</div>
<div>existing&nbsp;ones&nbsp;(like&nbsp;GTK)&nbsp;are&nbsp;single&nbsp;threaded;</div>
<div>thus&nbsp;we&nbsp;can't&nbsp;access&nbsp;GTK&nbsp;widgets&nbsp;(and&nbsp;data&nbsp;structures&nbsp;containing&nbsp;them),&nbsp;from&nbsp;inside&nbsp;actors;</div>
<div>solution:</div>
<div><a href='https://gtk-rs.org/docs/glib/source/fn.idle_add.html'>https://gtk-rs.org/docs/glib/source/fn.idle_add.html</a></div>
<div><a href='https://docs.rs/fragile/1.0.0/fragile/struct.Fragile.html'>https://docs.rs/fragile/1.0.0/fragile/struct.Fragile.html</a></div>
<div><a href='https://docs.rs/send_wrapper/0.4.0/send_wrapper/'>https://docs.rs/send_wrapper/0.4.0/send_wrapper/</a></div>
<div>we&nbsp;deal&nbsp;with&nbsp;Gobjects&nbsp;by&nbsp;wrapping&nbsp;them&nbsp;in&nbsp;a&nbsp;Gsend;</div>
<div>Gsend&nbsp;embeds&nbsp;a&nbsp;Fragile&nbsp;which&nbsp;will&nbsp;be&nbsp;created&nbsp;in&nbsp;the&nbsp;GTK&nbsp;main&nbsp;thread&nbsp;(using&nbsp;"idle_add"),</div>
<div>&nbsp;&nbsp;and&nbsp;then&nbsp;received&nbsp;(and&nbsp;put&nbsp;inside&nbsp;the&nbsp;Gsend)&nbsp;using&nbsp;a&nbsp;channel;</div>
<div>Gsend.new&nbsp;gets&nbsp;a&nbsp;closure&nbsp;(instead&nbsp;of&nbsp;a&nbsp;Gobject&nbsp;value);</div>
<div>&nbsp;&nbsp;the&nbsp;closure's&nbsp;return&nbsp;type&nbsp;is&nbsp;the&nbsp;type&nbsp;of&nbsp;the&nbsp;specific&nbsp;Gobject;</div>
<div></div>
<div><a href='https://gtk-rs.org/docs/gtk/'>https://gtk-rs.org/docs/gtk/</a></div>
<div><a href='https://mmstick.github.io/gtkrs-tutorials/introduction.html'>https://mmstick.github.io/gtkrs-tutorials/introduction.html</a></div>
<div><a href='https://github.com/gtk-rs/examples/tree/master/src/bin'>https://github.com/gtk-rs/examples/tree/master/src/bin</a></div>
<div></div>
<div>gtksourceview4&nbsp;webkit2gtk</div>
<div>poppler-glib&nbsp;goffice&nbsp;goocanvas</div>
<div></div>
<div><a href='https://pijul.org/'>https://pijul.org/</a></div>
<div></div>
<div><a href='https://arcolinuxiso.com/how-to-create-your-own-online-arch-linux-repository-on-github-and-use-it-on-any-arcolinux/'>https://arcolinuxiso.com/how-to-create-your-own-online-arch-linux-repository-on-github-and-use-it-on-any-arcolinux/</a></div>
<div><a href='https://wiki.archlinux.org/index.php/unofficial_user_repositories'>https://wiki.archlinux.org/index.php/unofficial_user_repositories</a></div></pre>