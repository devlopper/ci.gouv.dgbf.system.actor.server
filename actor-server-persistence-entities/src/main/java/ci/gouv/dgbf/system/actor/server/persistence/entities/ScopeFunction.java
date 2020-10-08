package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;
import java.util.Collection;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.validation.constraints.NotNull;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.object.__static__.persistence.AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl;
import org.cyk.utility.__kernel__.persistence.query.EntityFinder;
import org.cyk.utility.__kernel__.string.StringHelper;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@Entity @Table(name=ScopeFunction.TABLE_NAME)
public class ScopeFunction extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@ManyToOne @JoinColumn(name = COLUMN_SCOPE) @NotNull private Scope scope;
	@ManyToOne @JoinColumn(name = COLUMN_FUNCTION) @NotNull private Function function;
	@Column(name = COLUMN_NUMBER_OF_ACTOR) private Integer numberOfActor;
	
	@Transient private String scopeAsString;
	@Transient private String functionAsString;
	@Transient private Boolean shared;
	@Transient private String sharedAsString;
	
	@Override
	public ScopeFunction setIdentifier(String identifier) {
		return (ScopeFunction) super.setIdentifier(identifier);
	}
	
	@Override
	public ScopeFunction setCode(String code) {
		return (ScopeFunction) super.setCode(code);
	}
	
	@Override
	public ScopeFunction setName(String name) {
		return (ScopeFunction) super.setName(name);
	}
	
	public ScopeFunction setScopeFromIdentifier(String identifier) {
		if(StringHelper.isBlank(identifier))
			setScope(null);
		else
			setScope(EntityFinder.getInstance().find(Scope.class, identifier));
		return this;
	}
	
	public ScopeFunction setFunctionFromIdentifier(String identifier) {
		if(StringHelper.isBlank(identifier))
			setFunction(null);
		else
			setFunction(EntityFinder.getInstance().find(Function.class, identifier));
		return this;
	}

	public static Boolean isExistsByCodesOnly(Collection<ScopeFunction> scopeFunctions,Scope scope,Function function) {
		if(CollectionHelper.isEmpty(scopeFunctions) || scope == null || function == null)
			return null;
		for(ScopeFunction scopeFunction : scopeFunctions)
			if(scope.getCode().equals(scopeFunction.getScopeAsString()) && function.getCode().equals(scopeFunction.getFunctionAsString()))
				return Boolean.TRUE;
		return Boolean.FALSE;
	}
	
	public static final String FIELD_SCOPE = "scope";
	public static final String FIELD_FUNCTION = "function";
	public static final String FIELD_SCOPE_AS_STRING = "scopeAsString";
	public static final String FIELD_FUNCTION_AS_STRING = "functionAsString";
	public static final String FIELD_NUMBER_OF_ACTOR = "numberOfActor";
	public static final String FIELD_SHARED = "shared";
	public static final String FIELD_SHARED_AS_STRING = "sharedAsString";
	
	public static final String TABLE_NAME = "POSTE";
	
	public static final String COLUMN_SCOPE = "domaine";
	public static final String COLUMN_FUNCTION = "fonction";
	public static final String COLUMN_NUMBER_OF_ACTOR = "nombre_acteur";
}