package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import java.io.Serializable;

import org.cyk.utility.persistence.query.Querier;
import org.cyk.utility.persistence.server.Helper;
import org.cyk.utility.persistence.server.query.string.QueryStringBuilder;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Profile;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true)
public class ProfileRequestableAndRequestableAsStringsReader extends AbstractProfileReaderImpl implements Serializable {

	@Override
	protected String getQueryValue() {
		QueryStringBuilder.Arguments arguments = new QueryStringBuilder.Arguments();
		arguments.getProjection(Boolean.TRUE).addFromTuple("t",Profile.FIELD_IDENTIFIER,Profile.FIELD_REQUESTABLE);
		arguments.getTuple(Boolean.TRUE).add("Profile t");
		arguments.getPredicate(Boolean.TRUE).add("t.identifier IN :"+Querier.PARAMETER_NAME_IDENTIFIERS);
		return QueryStringBuilder.getInstance().build(arguments);
	}
	
	@Override
	protected void __set__(Profile profile, Object[] array) {
		Integer index = 1;
		profile.setRequestable((Boolean) array[index++]);
		profile.setRequestableAsString(Helper.ifTrueYesElseNo(profile.getRequestable()));
	}
}