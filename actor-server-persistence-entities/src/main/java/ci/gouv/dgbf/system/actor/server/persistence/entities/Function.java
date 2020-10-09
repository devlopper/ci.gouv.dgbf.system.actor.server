package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;
import java.util.Collection;

import javax.persistence.Cacheable;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.validation.constraints.NotNull;

import org.cyk.utility.__kernel__.object.__static__.persistence.AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl;
import org.cyk.utility.__kernel__.persistence.query.EntityFinder;
import org.cyk.utility.__kernel__.string.StringHelper;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@Entity @Table(name=Function.TABLE_NAME)
@Cacheable(value = true)
public class Function extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@ManyToOne @JoinColumn(name = COLUMN_TYPE) @NotNull private FunctionType type;
	@Column(name = COLUMN_NUMBER_OF_ACTOR_PER_SCOPE) private Integer numberOfActorPerScope;
	//@Column(name = COLUMN_NUMBER_OF_ACTOR_PER_SCOPE) private String scopeFunctionCodeGenerationScript;
	//@Column(name = COLUMN_NUMBER_OF_ACTOR_PER_SCOPE) private String scopeFunctionNameGenerationScript;
	
	@Transient private Collection<String> profilesAsStrings;
	@Transient private Collection<String> scopesAsStrings;
	@Transient private String profilesAsString;
	@Transient private Integer numberOfScopes;
	@Transient private String scopesAsString;
	@Transient private String scopeTypesAsString;
	@Transient private Collection<ScopeType> scopeTypes;
	@Transient private Boolean shared;
	@Transient private String sharedAsString;
	
	@Override
	public Function setIdentifier(String identifier) {
		return (Function) super.setIdentifier(identifier);
	}
	
	@Override
	public Function setCode(String code) {
		return (Function) super.setCode(code);
	}
	
	@Override
	public Function setName(String name) {
		return (Function) super.setName(name);
	}
	
	public Function setTypeFromIdentifier(String identifier) {
		if(StringHelper.isBlank(identifier))
			setType(null);
		else
			setType(EntityFinder.getInstance().find(FunctionType.class, identifier));
		return this;
	}
	
	public static final String FIELD_TYPE = "type";
	public static final String FIELD_PROFILES_AS_STRINGS = "profilesAsStrings";
	public static final String FIELD_PROFILES_AS_STRING = "profilesAsString";
	public static final String FIELD_SCOPES_AS_STRINGS = "scopesAsString";
	public static final String FIELD_NUMBER_OF_SCOPES = "numberOfScopes";
	public static final String FIELD_SCOPE_TYPES_AS_STRING = "scopeTypesAsString";
	public static final String FIELD_SCOPE_TYPES = "scopeTypes";
	public static final String FIELD_NUMBER_OF_ACTOR_PER_SCOPE = "numberOfActorPerScope";
	public static final String FIELD_SHARED = "shared";
	public static final String FIELD_SHARED_AS_STRING = "sharedAsString";
	
	public static final String COLUMN_TYPE = "type";
	public static final String COLUMN_NUMBER_OF_ACTOR_PER_SCOPE = "nombre_acteur_par_poste";
	
	public static final String TABLE_NAME = "FONCTION";	
	
	public static final String CODE_ADMINISTRATEUR = "ADMINISTRATEUR";
}