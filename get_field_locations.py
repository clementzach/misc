in_file = 'CCC Client Insurance Info.pdf'
out_file = 'extracted_information_by_location.csv'





#f = open(out_file, "w")


from pdfminer.high_level import extract_pages

from pdfminer.layout import LTTextContainer, LAParams, LTChar, LTLine, LTPage


element_list = []
num_pages = 1
for page_layout in extract_pages(in_file):
    #print(dir(page_layout))
    #print(help(page_layout.set_bbox))
    #print(page_layout.x0)
    if num_pages > 20: ## only do the first few while testing
        break
    for element in page_layout:
        element_list.append(element) #for using dir()
        
        if isinstance(element, LTTextContainer):
            #first_char = True
            #if first_char:
                #for text_line in element:
                    #print(dir(text_line))
                    #print('text line height ' + str(text_line.height))
                    #for character in text_line:
                        #if isinstance(character, LTChar):
                            #print('font size ' + str(character.size))
                            #print('font height ' + str(character.height))
                            #print('font width ' + str(character.width))
                            #print(character.fontname)
                            #print(dir(character))
                            #first_char = False
                            #break
            
            #print(element.font_size)
            print('x0 ' + str(element.x0))
            #try:
            #    print('x1 ' + str(element.x1))
            #except:
            #    print('x1 not available')
            #print('y0 ' + str(element.y0))
            try:
                print('y1 ' + str(element.y1))
            except:
                print('y1 not available')
            print(element.get_text())
            print('')
            print('')
    num_pages += 1




            
