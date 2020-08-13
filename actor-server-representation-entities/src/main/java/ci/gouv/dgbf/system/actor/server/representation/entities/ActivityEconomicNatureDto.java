package ci.gouv.dgbf.system.actor.server.representation.entities;

import java.io.Serializable;

import org.cyk.utility.__kernel__.object.__static__.representation.AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
public class ActivityEconomicNatureDto extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	private String sectionAsString;	
	private String budgetSpecializationUnitAsString;	
	private String actionAsString;	
	private String activityAsString;	
	private String economicNatureAsString;
	
	@Override
	public ActivityEconomicNatureDto setIdentifier(String identifier) {
		return (ActivityEconomicNatureDto) super.setIdentifier(identifier);
	}

}