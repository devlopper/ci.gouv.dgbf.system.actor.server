package ci.gouv.dgbf.system.actor.server.representation.entities;

import java.io.Serializable;

import org.cyk.utility.__kernel__.object.__static__.representation.AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Locality.Type;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
public class LocalityDto extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	private Type type;
	private LocalityDto parent;
	
	private String regionIdentifier;
	private String regionCodeName;
	
	private String departmentIdentifier;
	private String departmentCodeName;
	
	private LocalityDto region;
	private LocalityDto department;
	
	@Override
	public LocalityDto setIdentifier(String identifier) {
		return (LocalityDto) super.setIdentifier(identifier);
	}
}