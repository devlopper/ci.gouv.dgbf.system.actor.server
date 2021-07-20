package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.persistence.query.Querier;
import org.cyk.utility.persistence.server.Helper;
import org.cyk.utility.persistence.server.query.string.QueryStringBuilder;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true)
public abstract class AbstractScopeVisiblesReader extends AbstractScopeReaderImpl implements Serializable {

	protected Boolean stringify;
	
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
	public void set(Collection<Scope> scopes, Collection<Object[]> arrays) {
		super.set(scopes, arrays);
		if(CollectionHelper.isNotEmpty(scopes)) {
			if(Boolean.TRUE.equals(stringify))
				scopes.forEach(scope -> {
					scope.setVisibleAsString(Helper.ifTrueYesElseNo(scope.getVisible()));
				});
		}
	}
	
	@Override
	protected void __set__(Scope scope, Object[] array) {
		scope.setVisible(Boolean.TRUE);
	}
}