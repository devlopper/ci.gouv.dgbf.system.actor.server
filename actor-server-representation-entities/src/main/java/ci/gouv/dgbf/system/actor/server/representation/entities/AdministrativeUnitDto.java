package ci.gouv.dgbf.system.actor.server.representation.entities;

import java.io.Serializable;

import org.cyk.utility.__kernel__.object.__static__.representation.AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
public class AdministrativeUnitDto extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl implements Serializable {
	private static final long serialVersionUID = 1L;

	private SectionDto section;
	private String sectionCodeName;
	private String sectionAsString;
	private String sectionIdentifier;
	
	private LocalityDto subPrefecture;
	private LocalityDto department;
	private LocalityDto region;
	
	@Override
	public AdministrativeUnitDto setIdentifier(String identifier) {
		return (AdministrativeUnitDto) super.setIdentifier(identifier);
	}

}