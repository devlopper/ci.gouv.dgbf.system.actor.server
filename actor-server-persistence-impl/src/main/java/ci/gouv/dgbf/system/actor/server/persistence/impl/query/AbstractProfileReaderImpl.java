package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import org.cyk.utility.persistence.server.query.ArraysReaderByIdentifiers;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Profile;

public abstract class AbstractProfileReaderImpl extends ArraysReaderByIdentifiers.AbstractImpl.DefaultImpl<Profile> {

	@Override
	protected Class<Profile> getEntityClass() {
		return Profile.class;
	}
}