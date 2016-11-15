
## Android Support Library ##

As it turns out, linking with the Android Support Library is more complicated than you would think. So I'm
including these instructions in the hopes that it helps everyone else figure it out. We won't be going over
this in the lab because it's a really messy sequence of steps that takes a little while to get right.

The Android Support Library is a way to get newer Android features in your app without losing support for older
devices that don't have them. Now, there are *plenty* of tutorials online on how to get the Support Library to
work with your app if you're using Android Studio, but Kawa users can't use Android Studio for obvious reasons.

What we'd like to do here is get the Design library to work. These instructions should apply to any of the
libraries, with some minor modifications.

1. Download the Support Library

   Fortunately, this step is easy, since it's the same as if you were using Android Studio. Just open up the
   SDK Manager (run `android` in your terminal), go down to Extras, and select Android Support Repository. That
   should be the only additional download you'll need.

2. Move the library you want into your libs directory

   This is the first of many unintuitive steps in this process. You need to make a copy of the library you want
   in your libs directory. No, don't link to it; make an actual copy. In this case, we need the design library,
   which is located in `$ANDROID_SDK/extras/android/support/`. Copy the folder called `design` into your libs
   directory inside your project.

3. Add the library to the properties file

   In your `project.properties` file, add a line at the bottom that says
   `android.library.reference.1=./libs/design`.

4. Turn the library into a subproject

   You need the `android` tool to recognize the library as... well, a library. Go into the libs directory and
   run the command `android update lib-project --target 3 --path design`. This will create properties files
   for the design library. But the properties aren't quite right. Go into
   `$PROJECT_DIR/libs/design/project.properties` and make sure the target is the same as the target in your
   main project. Then add the line `android.library=true` to the end.

5. Set up AppCompat

   If you were to build the project right now, you would get annoyingly cryptic error messages. That's Android's
   unique way of informing you that you're missing some dependencies. Specifically, AppCompat. So we're going to
   follow Steps 2-4 for the AppCompat library. It's located at `$ANDROID_SDK/extras/android/support/v7` and
   needs to be copied to the libs directory as well.

   AppCompat will need the same changes that Design got. So go into libs and run
   `android update lib-project --target 4 --path appcompat`. The target identifier needs to be different
   than the one used for Design. Then you need to go into `$PROJECT_DIR/libs/appcompat/project.properties`
   and make the target sync up with your main project target. You'll also add `android.library=true` to this
   one as well.

6. Inform the libraries of one another

   We installed AppCompat so that Design would work. So we need to go back to the `project.properties` file for
   Design and add `android.library.reference.1=../appcompat` to inform it that AppCompat does in fact exist.

   Further, we need to go tell the base project about this new library. Since Design depends on AppCompat,
   AppCompat must be listed first. So you need to change the `android.library.reference.1` we added earlier
   to `android.library.reference.2` and add, above that, the line `android.library.reference.1=./libs/appcompat`.

7. Change your minimum SDK

   The Support Libraries aren't compatible with some of the first few Android versions, so you need to change your
   minimum supported SDK to account for this. In the `AndroidManifest.xml` file for your base project, immediately
   above the `<application ... >` line, add the following:
   `<uses-sdk android:minSdkVersion="7" android:targetSdkVersion="NN" />`, where NN is the SDK version that you
   are compiling your project against. API 7 is (as far as I can tell) the oldest Android version which supports
   all of the libraries we've used in this example.

8. Try to build the project

   There's another thing we have to do, but first, we should try to build the project from scratch. If everything
   so far is working, the project should compile but might crash at runtime. So run `ant clean debug` and see
   what happens. If everything compiles, great! Go onto the final step.

9. Modify the classpath

   The problem now is that you've got all these awesome libraries in your app, but Kawa doesn't know how
   to compile with them. To fix that, we need to modify how Kawa compiles your source code. In your project's
   main directory, you should have a `custom_rules.xml` file. Open that file and look for the `<classpath>` tag.
   There should be a list of `<pathelement>` tags within it. You want to add more `<pathelement>` tags to
   account for the new jar files.

   ```xml
   <pathelement path="libs/appcompat/libs/android-support-v4.jar" />
   <pathelement path="libs/design/libs/android-support-design.jar" />
   <pathelement path="libs/appcompat/libs/android-support-v7-appcompat.jar" />
   ```

   Once that's done, you should be good as gold. Compile and run the project, and to see if the Support Library is
   actually there, try to code in a
   [Snackbar object](https://developer.android.com/training/snackbar/showing.html), which is a Support Library
   tool for showing app messages.

So yeah, it's a bit of a mess. It's clear that the Support Library wasn't built to work with non-Android Studio
users, but it is possible to get it to work. So if you're trying this yourself, good luck to you, and I hope this
was able to help a bit!
