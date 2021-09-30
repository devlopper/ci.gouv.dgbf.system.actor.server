package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import org.cyk.utility.persistence.server.query.ArraysReaderByIdentifiers;

import ci.gouv.dgbf.system.actor.server.persistence.entities.AdministrativeUnit;

public abstract class AbstractAdministrativeUnitReaderImpl extends ArraysReaderByIdentifiers.AbstractImpl.DefaultImpl<AdministrativeUnit> {

	@Override
	protected Class<AdministrativeUnit> getEntityClass() {
		return AdministrativeUnit.class;
	}
}