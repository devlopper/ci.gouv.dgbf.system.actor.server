package ci.gouv.dgbf.system.actor.server.representation.entities;

import java.io.Serializable;

import org.cyk.utility.__kernel__.object.__static__.representation.AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
public class MenuDto extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	private String moduleAsString;
	private String moduleCodeName;
	private String serviceAsString;
	private String serviceCodeName;
	private String uniformResourceIdentifier;
	private Boolean defined;
	private String status;
	private String profilesAsString;
	
	@Override
	public MenuDto setIdentifier(String identifier) {
		return (MenuDto) super.setIdentifier(identifier);
	}

}