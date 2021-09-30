package ci.gouv.dgbf.system.actor.server.representation.entities;

import java.io.Serializable;

import org.cyk.utility.__kernel__.object.__static__.representation.AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
public class ScopeDto extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	private ScopeTypeDto type;
	private ActorDto actor;
	private String actorIdentifier;
	private ScopeDto section;
	private Boolean visible;
	private String sectionAsString,actionAsString,budgetSpecializationUnitAsString,activityAsString,activityCategoryAsString,visibleAsString,typeAsString
	,actorAsString;
	
	@Override
	public ScopeDto set__deletable__(Boolean __deletable__) {
		return (ScopeDto) super.set__deletable__(__deletable__);
	}
	
	@Override
	public ScopeDto setIdentifier(String identifier) {
		return (ScopeDto) super.setIdentifier(identifier);
	}
	
	@Override
	public String toString() {
		return code+" | "+name+" | "+type;
	}
	
	/**/
	
	public static final String JSON_FIELD_IDENTIFIER = "identifiant";
	public static final String JSON_FIELD_CODE = "code";
	public static final String JSON_FIELD_NAME = "libelle";
	public static final String JSON_FIELD_SECTION_IDENTIFIER = "identifiant_section";
	public static final String JSON_FIELD_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER = "identifiant_unite_specialisation_budget";
}