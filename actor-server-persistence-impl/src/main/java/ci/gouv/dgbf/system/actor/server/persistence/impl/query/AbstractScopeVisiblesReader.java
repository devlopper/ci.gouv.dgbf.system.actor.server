package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import java.io.Serializable;

import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.persistence.query.Querier;
import org.cyk.utility.persistence.server.query.string.QueryStringBuilder;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeType;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true)
public abstract class AbstractScopeVisiblesReader extends AbstractScopeReaderImpl implements Serializable {

	@Override
	protected String getQueryValue() {
		QueryStringBuilder.Arguments arguments = new QueryStringBuilder.Arguments();
		arguments.getProjection(Boolean.TRUE).addFromTuple("t",Scope.FIELD_IDENTIFIER,Scope.FIELD_IDENTIFIER);
		arguments.getTuple(Boolean.TRUE).add("Scope t");
		arguments.getPredicate(Boolean.TRUE).ands("t.identifier IN :"+Querier.PARAMETER_NAME_IDENTIFIERS,getVisiblePredicate());
		return QueryStringBuilder.getInstance().build(arguments);
	}
	
	protected abstract String getVisiblePredicate();
	
	@Override
	protected void __set__(Scope scope, Object[] array) {
		//Integer index = 1;
		scope.setVisible(Boolean.TRUE);
	}
	
	/**/
	
	public static AbstractScopeVisiblesReader instantiate(String typeCode,String actorCode,Boolean negate) {
		if(StringHelper.isBlank(actorCode)) {			
			if(ScopeType.CODE_SECTION.equals(typeCode))
				return new ScopeSectionVisiblesReader();
			if(ScopeType.CODE_UA.equals(typeCode))
				return new ScopeAdministrativeUnitVisiblesReader();
			if(ScopeType.CODE_USB.equals(typeCode))
				return new ScopeBudgetSpecializationUnitVisiblesReader();
			if(ScopeType.CODE_ACTION.equals(typeCode))
				return new ScopeActionVisiblesReader();
			if(ScopeType.CODE_ACTIVITE.equals(typeCode))
				return new ScopeActivityVisiblesReader();
		}else {
			if(ScopeType.CODE_SECTION.equals(typeCode))
				return new ScopeSectionVisiblesByActorReader();
			if(ScopeType.CODE_UA.equals(typeCode))
				return new ScopeAdministrativeUnitVisiblesByActorReader();
			if(ScopeType.CODE_USB.equals(typeCode))
				return new ScopeBudgetSpecializationUnitVisiblesByActorReader();
			if(ScopeType.CODE_ACTION.equals(typeCode))
				return new ScopeActionVisiblesByActorReader();
			if(ScopeType.CODE_ACTIVITE.equals(typeCode))
				return new ScopeActivityVisiblesByActorReader();
		}		
		throw new RuntimeException(String.format("No visibles reader found for type coded <<%s>>",typeCode));
	}
}