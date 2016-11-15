
# FPC Calculator #

This is a lab that will eventually create a calculator. If you want to skip to the end, the result is in the
`complete` folder, but the partial version that we will be working with at the meeting is the `lab` folder.

## Before the Lab ##

There are a few things you will need, and since waiting for all of us to download the Android SDK isn't the
most exciting thing in the world, I'll be leaving instructions here for how to set your machine up to be
ready to work with Kawa.

### Java ###

The first thing you'll need is a JDK. It's important to note that **Java 8 will absolutely not work**. Believe
me, I wanted it to. But if you try to use Java 8, you will get cryptic error messages and not get anything
done. This is an Android thing, not a Kawa thing. Fortunately, Java 7 and 8 will install beside one another
and play nice. All you have to do is make sure the JAVA_HOME environment variable points to Java 7, not 8.

### Android ###

Once you have Java, you'll need to get Android. Follow [this link](https://developer.android.com/studio/index.html)
to download Android. You don't need Android Studio, so unless you want the IDE for some reason, just scroll down
to the bottom where it says "Get just the command line tools" and use those downloads. You'll want to make sure
the utilities `android` and `adb` are on your path. Also, if you don't have an Android phone or tablet of your
own, you'll need the `emulator` utility as well.

Once you've installed Android, you'll want to run `android` (with no arguments) on the command line. This will
give you a GUI where you can download SDK packages. A few things are checked by default; you want all of these.
Additionally, for this lab we'll be compiling with API level 23, so you'll want to check everything in the API
23 folder.

*NOTE: This lab is being compiled against the API 23 (Marshmallow) SDK and is designed to run on any devices
running API 14 (Ice Cream Sandwich) or newer. Unfortunately, pre-Ice Cream Sandwich devices do not have the
GridLayout which is necessary for positioning the buttons of the calculator, so if your device is older than
2011, you may have to use an emulator.*

### Ant ###

Now, unfortunately, the official Android build tool (Gradle) doesn't play nice with things that aren't Java,
so we'll be using a different build tool. The Kawa developers recommend Apache Ant, which can be downloaded from
[their website](http://ant.apache.org/).

### An Android Device ###

Now, if you have an Android device, great. Just don't forget to bring it (and a USB cable) with you this
Thursday. You'll also want to enable Developer Options and USB Debugging on your device, and some smartphones
require that you establish a handshake with the laptop you want to develop on. Unfortunately, how to go
about doing this differs from smartphone to smartphone, so I recommend Googling your smartphone model.

If you don't have an Android device, that's fine too. You can emulate one on your computer. Simply run `android`,
then in the Tools menu, click "Manage AVDs" and create an AVD. You'll want to name it something short and
alphanumeric, because we'll be invoking your emulator from the command line. We don't need any special
hardware features, so just pick a simple model and give it a moderate amount of RAM. Once you've set up
and saved the AVD, you can run your emulator asynchronously with the command `emulator -avd nameofyouremulator &`.

Whether you're using a physical smartphone or an emulator, once you've got everything set up, there's an easy
way to see if it's working. Simply run `adb devices`. It will show any emulators you have running as well as
any Android devices you have plugged in. If it lists your device, you're probably done. If it says "unknown"
next to your device, you might need to go into your phone's settings and tell it to trust your computer.

### Kawa ###

The last step is Kawa. Fortunately, unless you're running a particularly strange operating system, this should
be an easy step. Simply go into the `./lab/libs` directory and run the command `java kawa.repl`. If a REPL
window pops up, you're done. If it doesn't, you'll need to build Kawa for your machine. The instructions to
do that are available [here](https://www.gnu.org/software/kawa/Building-for-Android.html).

To see if you've got everything configured correctly, try running the calculator. From the project's main
directory, run `ant debug`. This will produce a debug version of the app. You cannot upload debug versions
to the app store; if you wanted to produce the finished version, you would need to go through some additional
steps, but that's well out of the scope of this lab. Once the app is built, start up your emulator or plug
in your physical device and use `adb devices` to make sure it's visible to the computer. Then run
`adb install -r ./bin/FPCCalculator-debug.apk`. Once that process finishes, you should be able to go into
your apps menu on your device and find the calculator under "FPC Calculator". Assuming you compiled the lab
version and not the completed version, you should see a calculator in which the buttons don't do anything
when you tap them.
