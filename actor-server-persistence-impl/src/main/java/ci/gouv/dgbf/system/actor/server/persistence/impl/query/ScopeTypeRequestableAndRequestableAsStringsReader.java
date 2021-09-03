package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import java.io.Serializable;

import org.cyk.utility.persistence.query.Querier;
import org.cyk.utility.persistence.server.Helper;
import org.cyk.utility.persistence.server.query.string.QueryStringBuilder;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeType;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true)
public class ScopeTypeRequestableAndRequestableAsStringsReader extends AbstractScopeTypeReaderImpl implements Serializable {

	@Override
	protected String getQueryValue() {
		QueryStringBuilder.Arguments arguments = new QueryStringBuilder.Arguments();
		arguments.getProjection(Boolean.TRUE).addFromTuple("t",ScopeType.FIELD_IDENTIFIER,ScopeType.FIELD_REQUESTABLE);
		arguments.getTuple(Boolean.TRUE).add("ScopeType t");
		arguments.getPredicate(Boolean.TRUE).add("t.identifier IN :"+Querier.PARAMETER_NAME_IDENTIFIERS);
		return QueryStringBuilder.getInstance().build(arguments);
	}
	
	@Override
	protected void __set__(ScopeType scopeType, Object[] array) {
		Integer index = 1;
		scopeType.setRequestable((Boolean) array[index++]);
		scopeType.setRequestableAsString(Helper.ifTrueYesElseNo(scopeType.getRequestable()));
	}
}