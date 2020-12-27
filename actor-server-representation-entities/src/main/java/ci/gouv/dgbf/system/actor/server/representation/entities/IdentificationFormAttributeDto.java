package ci.gouv.dgbf.system.actor.server.representation.entities;

import java.io.Serializable;

import org.cyk.utility.__kernel__.object.__static__.representation.AbstractIdentifiableSystemScalarStringImpl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
public class IdentificationFormAttributeDto extends AbstractIdentifiableSystemScalarStringImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	private IdentificationFormDto form;
	private String formAsString;
	private IdentificationAttributeDto attribute;
	private String attributeAsString;	
	private String name;
	private Integer orderNumber;
	private Boolean required;	
	private String requiredAsString;
	
	@Override
	public IdentificationFormAttributeDto setIdentifier(String identifier) {
		return (IdentificationFormAttributeDto) super.setIdentifier(identifier);
	}

}