# Небольшая памятка (для себя) по созданию новых релизов

1) Подтянуть все изменения, объединить ветки
   ```
   git checkout master
   git pull
   git merge develop
   ```
2) Обновить номер версии в 2-х местах. "Проект > параметры проекта > Сведения о версии".
   - Номера версий
   - Прочие сведения > ProductVersion (почему-то не обновляется само автоатически)

3) Сделать commit и push
   ```
   git commit -a -m 'vX.XX.X'
   git tag -a vX.XX.X
   git push
   git push origin vX.XX.X
   ```

4) Запустить сборку проекта: build.sh (linux) и build.bat (windows)

5) Когда всё собрано объединяем содержимое папки `build/releases/vx.xx.x/debug symbols` из windows и linux в один архив. Желательно 7zip с максимальным уровнем сжатия.

6) Находим [тут](https://github.com/artem78/AutoScreenshot/tags) последний тег, нажимаем `Create release from tag`
   - Пишем, что изменилось
     ```
     Changes:
     - Updated blablabla...
     - Fixed blablabla...
     ```
   - Прикрепляем полученные файлы (5 штук):
      - AutoScreenshot_vx.xx.x_Linux_x64_portable.zip
      - AutoScreenshot_vx.xx.x_Windows_setup.exe
      - AutoScreenshot_vx.xx.x_Windows_x64_portable.zip
      - AutoScreenshot_vx.xx.x_Windows_x86_portable.zip
      - debug.symbols.7z

   - Жмём кнопку `Publish release`

7) Вернуться на ту ветку, где были (рабочую ветку) и обновить её
   ```
   git checkout develop
   git merge master --ff-only
   git push
   ```

8) Обновить переводы на transifex [здесь](https://app.transifex.com/--334/autoscreenshot/dashboard/) (**todo: написать как**)

9) Обновить информацию на сайте https://artem78.github.io/AutoScreenshot

    ```
    cd Projects/Lazarus/AutoScreenshot/site
    cp content/1-23-1-released.md content/1-xx-x-released.md
    nano content/1-xx-x-released.md
   ```
   
   Меняем `title`, `date`, `changes`, сохраняем.
   
   Запускаем `update_current_version.py` (обновляет версию на странице `download`)
   
   Проверяем на локальной системе `./localtest.sh`
   
   Отправляем на хостинг `build_and_publish_on_production.sh`, сохряняем в git `git commit -a -m 'v1.xx'` `git push`



10) Всё, конец
