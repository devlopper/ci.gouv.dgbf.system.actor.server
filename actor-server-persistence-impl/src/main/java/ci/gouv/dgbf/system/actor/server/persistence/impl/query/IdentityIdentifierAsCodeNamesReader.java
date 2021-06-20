package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import org.cyk.utility.persistence.query.Language;
import org.cyk.utility.persistence.query.Querier;
import org.cyk.utility.persistence.server.query.ArraysReaderByIdentifiers;
import org.cyk.utility.persistence.server.query.string.QueryStringBuilder;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Identity;

public class IdentityIdentifierAsCodeNamesReader extends ArraysReaderByIdentifiers.AbstractImpl.DefaultImpl<Identity> {

	@Override
	protected String getQueryValue() {
		QueryStringBuilder.Arguments arguments = new QueryStringBuilder.Arguments();
		arguments.getProjection(Boolean.TRUE).addFromTuple("a",Actor.FIELD_CODE);
		arguments.getProjection(Boolean.TRUE).add(Language.Select.concat("t", Identity.FIELD_FIRST_NAME,Identity.FIELD_LAST_NAMES));
		arguments.getTuple(Boolean.TRUE).add("Identity t").addJoins("LEFT JOIN Actor a ON a.identity = t");
		arguments.getPredicate(Boolean.TRUE).add("a.code IN :"+Querier.PARAMETER_NAME_IDENTIFIERS);
		return QueryStringBuilder.getInstance().build(arguments);
	}
}