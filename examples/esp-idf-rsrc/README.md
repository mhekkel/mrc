Using resources in ESP-IDF
==========================

It is very easy to add resources to esp-idf projects as is shown in this example.
Here we create a single resource called `hello.txt` and its content comes from
the file `hello.txt`. The header file is created and resources are added to
the main component using the cmake snippet:

```cmake
# locate and include the mrc macro's
find_package(Mrc)

# write a header file
mrc_write_header(${CMAKE_CURRENT_SOURCE_DIR}/mrsrc.hpp)

# Add one resource to the executable
mrc_target_resources(${COMPONENT_LIB} CREATE_ELF_TEMPLATE
    RESOURCES ${CMAKE_CURRENT_SOURCE_DIR}/hello.txt)
```

Now you can simply use that resource:

```c++
#include "esp_log.h"

#include "mrsrc.hpp"

const char TAG[] = "esp-rsrc";

extern "C" void app_main(void)
{
    mrsrc::rsrc data("hello.txt");
    if (data)
    {
        std::string s(data.data(), data.size());
        ESP_LOGI(TAG, "Resource found: '%s'!", s.c_str());
    }
    else
        ESP_LOGI(TAG, "The resource was not found");
}
```

Use `idf.py build flash monitor` and the output will be:

```console
ESP-ROM:esp32h2-20221101
Build:Nov  1 2022
rst:0x1 (POWERON),boot:0xc (SPI_FAST_FLASH_BOOT)
SPIWP:0xee
mode:DIO, clock div:1
load:0x408460e0,len:0x17ac
load:0x4083cfd0,len:0xe88
load:0x4083efd0,len:0x2c94
entry 0x4083cfda
I (23) boot: ESP-IDF v5.3-dev-1353-gb3f7e2c8a4 2nd stage bootloader
I (24) boot: compile time Feb  1 2024 15:04:08
I (25) boot: chip revision: v0.1
I (28) boot.esp32h2: SPI Speed      : 64MHz
I (33) boot.esp32h2: SPI Mode       : DIO
I (38) boot.esp32h2: SPI Flash Size : 2MB
I (42) boot: Enabling RNG early entropy source...
I (48) boot: Partition Table:
I (51) boot: ## Label            Usage          Type ST Offset   Length
I (59) boot:  0 nvs              WiFi data        01 02 00009000 00006000
I (66) boot:  1 phy_init         RF data          01 01 0000f000 00001000
I (74) boot:  2 factory          factory app      00 00 00010000 00100000
I (81) boot: End of partition table
I (85) esp_image: segment 0: paddr=00010020 vaddr=42018020 size=0a0e8h ( 41192) map
I (107) esp_image: segment 1: paddr=0001a110 vaddr=40800000 size=05f08h ( 24328) load
I (116) esp_image: segment 2: paddr=00020020 vaddr=42000020 size=1734ch ( 95052) map
I (146) esp_image: segment 3: paddr=00037374 vaddr=40805f08 size=03acch ( 15052) load
I (152) esp_image: segment 4: paddr=0003ae48 vaddr=408099e0 size=011f4h (  4596) load
I (157) boot: Loaded app from partition at offset 0x10000
I (158) boot: Disabling RNG early entropy source...
I (174) cpu_start: Unicore app
W (183) clk: esp_perip_clk_init() has not been implemented yet
I (190) cpu_start: Pro cpu start user code
I (190) cpu_start: cpu freq: 96000000 Hz
I (191) heap_init: Initializing. RAM available for dynamic allocation:
I (195) heap_init: At 4080BDD0 len 000415B0 (261 KiB): RAM
I (201) heap_init: At 4084D380 len 00002B60 (10 KiB): RAM
I (209) spi_flash: detected chip: generic
I (212) spi_flash: flash io: dio
W (216) spi_flash: Detected size(4096k) larger than the size in the binary image header(2048k). Using the size in the binary image header.
I (229) cpu_start: Application information:
I (234) cpu_start: Project name:     my-rsrc-app
I (239) cpu_start: App version:      v1.3.10-23-gaff8ce4-dirty
I (246) cpu_start: Compile time:     Feb  1 2024 15:04:01
I (252) cpu_start: ELF file SHA256:  30e7d4161...
I (257) cpu_start: ESP-IDF:          v5.3-dev-1353-gb3f7e2c8a4
I (264) cpu_start: Min chip rev:     v0.0
I (269) cpu_start: Max chip rev:     v0.99 
I (273) cpu_start: Chip rev:         v0.1
I (279) sleep: Configure to isolate all GPIO pins in sleep state
I (285) sleep: Enable automatic switching of GPIO sleep configuration
I (292) main_task: Started on CPU0
I (292) main_task: Calling app_main()
I (292) esp-rsrc: Resource found: 'Hello, world!
'!
I (302) main_task: Returned from app_main()
```